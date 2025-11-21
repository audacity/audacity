/**********************************************************************

  Audacity: A Digital Audio Editor

  NyquistBase.cpp

  Dominic Mazzoni

**********************************************************************/
#include "NyquistBase.h"
#include "BasicUI.h"
#include "EffectManager.h"
#include "EffectOutputTracks.h"
#include "FileNames.h"
#include "LabelTrack.h"
#include "Languages.h"
#include "NoteTrack.h"
#include "PlatformCompatibility.h"
#include "PluginManager.h"
#include "Prefs.h"
#include "Project.h"
#include "ProjectRate.h"
#include "ShuttleAutomation.h"
#include "SpectrogramSettings.h"
#include "SyncLock.h"
#include "TempDirectory.h"
#include "TimeTrack.h"
#include "TimeWarper.h"
#include "ViewInfo.h"
#include "WaveChannelUtilities.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "wxFileNameWrapper.h"

#include "nyquist/xlisp/xlisp.h"
#include "nyx.h"

#include <cfloat>
#include <cstring>
#include <iostream>
#include <wx/log.h>
#include <wx/numformatter.h>
#include <wx/sstream.h>
#include <wx/tokenzr.h>
#include <wx/txtstrm.h>
#include <wx/wfstream.h>

int NyquistBase::mReentryCount = 0;

// Protect Nyquist from selections greater than 2^31 samples (bug 439)
#define NYQ_MAX_LEN (std::numeric_limits<int64_t>::max())

static const wxChar* KEY_Command = wxT("Command");
static const wxChar* KEY_Parameters = wxT("Parameters");

NyquistBase::NyquistBase(const wxString& fName)
    : mIsPrompt{fName == NYQUIST_PROMPT_ID}
{
    mAction = XO("Applying Nyquist Effect...");
    mExternal = false;
    mCompiler = false;
    mTrace = false;
    mRedirectOutput = false;
    mDebug = false;
    mIsSal = false;
    mOK = false;
    mAuthor = XO("n/a");
    mReleaseVersion = XO("n/a");
    mCopyright = XO("n/a");

    // set clip/split handling when applying over clip boundary.
    mRestoreSplits = true; // Default: Restore split lines.
    mMergeClips = -1;     // Default (auto):  Merge if length remains unchanged.

    mVersion = 4;

    mStop = false;
    mBreak = false;
    mCont = false;
    mIsTool = false;

    mMaxLen = NYQ_MAX_LEN;

    // Interactive Nyquist
    if (mIsPrompt) {
        mName = NYQUIST_PROMPT_NAME;
        mType = EffectTypeTool;
        mIsTool = true;
        mPromptName = mName;
        mPromptType = mType;
        mOK = true;
        return;
    }

    if (fName == NYQUIST_WORKER_ID) {
        // Effect spawned from Nyquist Prompt
        /* i18n-hint: It is acceptable to translate this the same as for "Nyquist
         * Prompt" */
        mName = XO("Nyquist Worker");
        return;
    }

    mFileName = fName;
    // Use the file name verbatim as effect name.
    // This is only a default name, overridden if we find a $name line:
    mName = Verbatim(mFileName.GetName());
    mFileModified = mFileName.GetModificationTime();
    ParseFile();

    if (!mOK && mInitError.empty()) {
        mInitError = XO("Ill-formed Nyquist plug-in header");
    }
}

NyquistBase::~NyquistBase()
{
}

// ComponentInterface implementation

PluginPath NyquistBase::GetPath() const
{
    if (mIsPrompt) {
        return NYQUIST_PROMPT_ID;
    }

    return mFileName.GetFullPath();
}

ComponentInterfaceSymbol NyquistBase::GetSymbol() const
{
    if (mIsPrompt) {
        return { NYQUIST_PROMPT_ID, NYQUIST_PROMPT_NAME }
    }

    return mName;
}

VendorSymbol NyquistBase::GetVendor() const
{
    if (mIsPrompt) {
        return XO("Audacity");
    }

    return mAuthor;
}

wxString NyquistBase::GetVersion() const
{
    // Are Nyquist version strings really supposed to be translatable?
    // See commit a06e561 which used XO for at least one of them
    return mReleaseVersion.Translation();
}

TranslatableString NyquistBase::GetDescription() const
{
    return mCopyright;
}

ManualPageID NyquistBase::ManualPage() const
{
    return mIsPrompt ? wxString("Nyquist_Prompt") : mManPage;
}

std::pair<bool, FilePath> NyquistBase::CheckHelpPage() const
{
    auto paths = NyquistBase::GetNyquistSearchPath();
    wxString fileName;

    for (size_t i = 0, cnt = paths.size(); i < cnt; i++) {
        fileName = wxFileName(paths[i] + wxT("/") + mHelpFile).GetFullPath();
        if (wxFileExists(fileName)) {
            return { true, fileName };
        }
    }
    return { false, wxEmptyString };
}

FilePath NyquistBase::HelpPage() const
{
    return mHelpPage;
}

// EffectDefinitionInterface implementation

EffectType NyquistBase::GetType() const
{
    return mType;
}

EffectType NyquistBase::GetClassification() const
{
    if (mIsTool) {
        return EffectTypeTool;
    }
    return mType;
}

EffectFamilySymbol NyquistBase::GetFamily() const
{
    return NYQUISTEFFECTS_FAMILY;
}

bool NyquistBase::IsInteractive() const
{
    if (mIsPrompt) {
        return true;
    }

    return mControls.size() != 0;
}

bool NyquistBase::IsDefault() const
{
    return mIsPrompt;
}

bool NyquistBase::EnablesDebug() const
{
    return mDebugButton;
}

bool NyquistBase::VisitSettings(
    SettingsVisitor& visitor, EffectSettings& settings)
{
    if (auto pSa = dynamic_cast<ShuttleSetAutomation*>(&visitor)) {
        LoadSettings(*pSa->mpEap, settings);
    }
    return true;
}

bool NyquistBase::VisitSettings(
    ConstSettingsVisitor& visitor, const EffectSettings& settings) const
{
    // For now we assume Nyquist can do get and set better than VisitSettings
    // can, And so we ONLY use it for getting the signature.
    if (auto pGa = dynamic_cast<ShuttleGetAutomation*>(&visitor)) {
        SaveSettings(settings, *pGa->mpEap);
        return true;
    }

    // Get the "definition," only for the help or info commands
    if (mExternal) {
        return true;
    }

    if (mIsPrompt) {
        visitor.Define(mInputCmd, KEY_Command, wxString {});
        visitor.Define(mParameters, KEY_Parameters, wxString {});
        return true;
    }

    for (const auto& ctrl : mControls) {
        double d = ctrl.val;

        if (d == UNINITIALIZED_CONTROL && ctrl.type != NYQ_CTRL_STRING) {
            d = GetCtrlValue(ctrl.valStr);
        }

        if (
            ctrl.type == NYQ_CTRL_FLOAT || ctrl.type == NYQ_CTRL_FLOAT_TEXT
            || ctrl.type == NYQ_CTRL_TIME) {
            visitor.Define(
                d, static_cast<const wxChar*>(ctrl.var.c_str()), (double)0.0,
                ctrl.low, ctrl.high, 1.0);
        } else if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_INT_TEXT) {
            int x = d;
            visitor.Define(
                x, static_cast<const wxChar*>(ctrl.var.c_str()), 0,
                static_cast<int>(ctrl.low), static_cast<int>(ctrl.high), 1);
            // parms.Write(ctrl.var, (int) d);
        } else if (ctrl.type == NYQ_CTRL_CHOICE) {
            // untranslated
            int x = d;
            // parms.WriteEnum(ctrl.var, (int) d, choices);
            visitor.DefineEnum(
                x, static_cast<const wxChar*>(ctrl.var.c_str()), 0,
                ctrl.choices.data(), ctrl.choices.size());
        } else if (ctrl.type == NYQ_CTRL_STRING || ctrl.type == NYQ_CTRL_FILE) {
            visitor.Define(
                ctrl.valStr, ctrl.var, wxString {}, ctrl.lowStr, ctrl.highStr);
            // parms.Write(ctrl.var, ctrl.valStr);
        }
    }
    return true;
}

bool NyquistBase::SaveSettings(
    const EffectSettings&, CommandParameters& parms) const
{
    if (mIsPrompt) {
        parms.Write(KEY_Command, mInputCmd);
        parms.Write(KEY_Parameters, mParameters);

        return true;
    }

    for (size_t c = 0, cnt = mControls.size(); c < cnt; c++) {
        const NyqControl& ctrl = mControls[c];
        double d = ctrl.val;

        if (d == UNINITIALIZED_CONTROL && ctrl.type != NYQ_CTRL_STRING) {
            d = GetCtrlValue(ctrl.valStr);
        }

        if (
            ctrl.type == NYQ_CTRL_FLOAT || ctrl.type == NYQ_CTRL_FLOAT_TEXT
            || ctrl.type == NYQ_CTRL_TIME) {
            parms.Write(ctrl.var, d);
        } else if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_INT_TEXT) {
            parms.Write(ctrl.var, (int)d);
        } else if (ctrl.type == NYQ_CTRL_CHOICE) {
            // untranslated
            parms.WriteEnum(
                ctrl.var, (int)d, ctrl.choices.data(), ctrl.choices.size());
        } else if (ctrl.type == NYQ_CTRL_STRING) {
            parms.Write(ctrl.var, ctrl.valStr);
        } else if (ctrl.type == NYQ_CTRL_FILE) {
            // Convert the given path string to platform-dependent equivalent
            resolveFilePath(const_cast<wxString&>(ctrl.valStr));
            parms.Write(ctrl.var, ctrl.valStr);
        }
    }

    return true;
}

bool NyquistBase::LoadSettings(
    const CommandParameters& parms, EffectSettings& settings) const
{
    // To do: externalize state so const_cast isn't needed
    return const_cast<NyquistBase*>(this)->DoLoadSettings(parms, settings);
}

bool NyquistBase::DoLoadSettings(
    const CommandParameters& parms, EffectSettings& settings)
{
    // Due to a constness problem that happens when using the prompt, we need
    // to be ready to switch the params to a local instance.
    const CommandParameters* pParms = &parms;
    CommandParameters localParms;

    if (mIsPrompt) {
        parms.Read(KEY_Command, &mInputCmd, wxEmptyString);
        parms.Read(KEY_Parameters, &mParameters, wxEmptyString);

        if (!mInputCmd.empty()) {
            ParseCommand(mInputCmd);
        }

        if (!mParameters.empty()) {
            pParms = &localParms;
            localParms.SetParameters(mParameters);
        }

        if (!IsBatchProcessing()) {
            mType = EffectTypeTool;
        }

        mPromptType = mType;
        mIsTool = (mPromptType == EffectTypeTool);
        mExternal = true;

        if (!IsBatchProcessing()) {
            return true;
        }
    }

    // Constants to document what the true/false values mean.
    const auto kTestOnly = true;
    const auto kTestAndSet = false;

    // badCount will encompass both actual bad values and missing values.
    // We probably never actually have bad values when using the dialogs
    // since the dialog validation will catch them.
    int badCount;
    // When batch processing, we just ignore missing/bad parameters.
    // We'll end up using defaults in those cases.
    if (!IsBatchProcessing()) {
        badCount = SetLispVarsFromParameters(*pParms, kTestOnly);
        if (badCount > 0) {
            return false;
        }
    }

    badCount = SetLispVarsFromParameters(*pParms, kTestAndSet);
    // We never do anything with badCount here.
    // It might be non zero, for missing parameters, and we allow that,
    // and don't distinguish that from an out-of-range value.
    return true;
}

// Sets the lisp variables form the parameters.
// returns the number of bad settings.
// We can run this just testing for bad values, or actually setting when
// the values are good.
int NyquistBase::SetLispVarsFromParameters(
    const CommandParameters& parms, bool bTestOnly)
{
    int badCount = 0;
    // First pass verifies values
    for (size_t c = 0, cnt = mControls.size(); c < cnt; c++) {
        NyqControl& ctrl = mControls[c];
        bool good = false;

        // This GetCtrlValue code is preserved from former code,
        // but probably is pointless.  The value d isn't used later,
        // and GetCtrlValue does not appear to have important needed
        // side effects.
        if (!bTestOnly) {
            double d = ctrl.val;
            if (d == UNINITIALIZED_CONTROL && ctrl.type != NYQ_CTRL_STRING) {
                d = GetCtrlValue(ctrl.valStr);
            }
        }

        if (
            ctrl.type == NYQ_CTRL_FLOAT || ctrl.type == NYQ_CTRL_FLOAT_TEXT
            || ctrl.type == NYQ_CTRL_TIME) {
            double val;
            good
                =parms.Read(ctrl.var, &val) && val >= ctrl.low && val <= ctrl.high;
            if (good && !bTestOnly) {
                ctrl.val = val;
            }
        } else if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_INT_TEXT) {
            int val;
            good
                =parms.Read(ctrl.var, &val) && val >= ctrl.low && val <= ctrl.high;
            if (good && !bTestOnly) {
                ctrl.val = (double)val;
            }
        } else if (ctrl.type == NYQ_CTRL_CHOICE) {
            int val;
            // untranslated
            good = parms.ReadEnum(
                ctrl.var, &val, ctrl.choices.data(), ctrl.choices.size())
                   && val != wxNOT_FOUND;
            if (good && !bTestOnly) {
                ctrl.val = (double)val;
            }
        } else if (ctrl.type == NYQ_CTRL_STRING || ctrl.type == NYQ_CTRL_FILE) {
            wxString val;
            good = parms.Read(ctrl.var, &val);
            if (good && !bTestOnly) {
                ctrl.valStr = val;
            }
        } else if (ctrl.type == NYQ_CTRL_TEXT) {
            // This "control" is just fixed text (nothing to save or restore),
            // Does not count for good/bad counting.
            good = true;
        }
        badCount += !good ? 1 : 0;
    }
    return badCount;
}

// Effect Implementation
bool NyquistBase::Init()
{
    // When Nyquist Prompt spawns an effect GUI, Init() is called for Nyquist
    // Prompt, and then again for the spawned (mExternal) effect.

    // EffectType may not be defined in script, so
    // reset each time we call the Nyquist Prompt.
    if (mIsPrompt) {
        mName = mPromptName;
        // Reset effect type each time we call the Nyquist Prompt.
        mType = mPromptType;
        mIsSpectral = false;
        mDebugButton = true; // Debug button always enabled for Nyquist Prompt.
        mEnablePreview
            =true; // Preview button always enabled for Nyquist Prompt.
        mVersion = 4;
    }

    // As of Audacity 2.1.2 rc1, 'spectral' effects are allowed only if
    // the selected track(s) are in a spectrogram view, and there is at
    // least one frequency bound and Spectral Selection is enabled for the
    // selected track(s) - (but don't apply to Nyquist Prompt).

    if (!mIsPrompt && mIsSpectral) {
        // Completely skip the spectral editing limitations if there is no
        // project because that is editing of macro parameters
        if (const auto project = FindProject()) {
            bool bAllowSpectralEditing = false;
            bool hasSpectral = false;
            for (auto t : TrackList::Get(*project).Selected<const WaveTrack>()) {
                // Find() not Get() to avoid creation-on-demand of views in case we
                // are only previewing
                const auto displays = GetDisplaysHook::Call(t);
                hasSpectral |= displays.end()
                               != std::find(
                    displays.begin(), displays.end(),
                    WaveChannelSubViewType {
                    WaveChannelViewConstants::Spectrum, {} });

                if (
                    hasSpectral
                    && (SpectrogramSettings::Get(*t).SpectralSelectionEnabled())) {
                    bAllowSpectralEditing = true;
                    break;
                }
            }

            if (!bAllowSpectralEditing || ((mF0 < 0.0) && (mF1 < 0.0))) {
                using namespace BasicUI;
                if (!hasSpectral) {
                    ShowMessageBox(
                        XO("Enable track spectrogram view before\n"
                           "applying 'Spectral' effects."),
                        MessageBoxOptions {}.IconStyle(Icon::Error));
                } else {
                    ShowMessageBox(
                        XO("To use 'Spectral effects', enable 'Spectral Selection'\n"
                           "in the track Spectrogram settings and select the\n"
                           "frequency range for the effect to act von."),
                        MessageBoxOptions {}.IconStyle(Icon::Error));
                }
                return false;
            }
        }
    }

    if (!mIsPrompt && !mExternal) {
        // TODO: (bugs):
        // 1) If there is more than one plug-in with the same name,
        // GetModificationTime may pick the wrong one. 2) If the ;type is changed
        // after the effect has been registered, the plug-in will appear in the
        // wrong menu.

        // TODO: If we want to auto-add parameters from spectral selection,
        // we will need to modify this test.
        // Note that removing it stops the caching of parameter values,
        //(during this session).
        if (mFileName.GetModificationTime().IsLaterThan(mFileModified)) {
            // If the effect has internal state, save and restore it.
            // If the effect is stateless, saving and restoring don't matter.
            auto dummySettings = MakeSettings();
            constexpr auto key = L"TemporarySettings";
            SaveUserPreset(key, dummySettings);

            mMaxLen = NYQ_MAX_LEN;
            ParseFile();
            mFileModified = mFileName.GetModificationTime();

            // Ignore failure
            (void)LoadUserPreset(key, dummySettings);
        }
    }

    return true;
}

static void RegisterFunctions();

//! Reads and writes Audacity's track objects, interchanging with Nyquist
//! sound objects (implemented in the library layer written in C)
struct NyquistBase::NyxContext
{
    using ProgressReport = std::function<bool (double)>;

    NyxContext(ProgressReport progressReport, double scale, double progressTot)
        : mProgressReport{move(progressReport)}
        , mScale{scale}
        , mProgressTot{progressTot}
    {
    }

    int GetCallback(
        float* buffer, int channel, int64_t start, int64_t len, int64_t totlen);
    int PutCallback(
        float* buffer, int channel, int64_t start, int64_t len, int64_t totlen);
    static int StaticGetCallback(
        float* buffer, int channel, int64_t start, int64_t len, int64_t totlen, void* userdata);
    static int StaticPutCallback(
        float* buffer, int channel, int64_t start, int64_t len, int64_t totlen, void* userdata);

    WaveTrack* mCurChannelGroup {};
    WaveChannel* mCurTrack[2] {};
    sampleCount mCurStart {};

    unsigned mCurNumChannels {}; //!< Not used in the callbacks

    using Buffer = std::unique_ptr<float[]>;
    Buffer mCurBuffer[2]; //!< used only in GetCallback
    sampleCount mCurBufferStart[2] {};
    size_t mCurBufferLen[2] {};
    sampleCount mCurLen {};

    WaveTrack::Holder mOutputTrack;

    double mProgressIn {};
    double mProgressOut {};

    const ProgressReport mProgressReport;
    const double mScale;
    const double mProgressTot;

    std::exception_ptr mpException {};
};

bool NyquistBase::Process(EffectInstance&, EffectSettings& settings)
{
    if (mIsPrompt && mControls.size() > 0 && !IsBatchProcessing()) {
        auto& nyquistSettings = GetSettings(settings);
        auto cleanup = finally([&] {
            // Free up memory
            nyquistSettings.proxySettings = {};
        });
        NyquistBase proxy { NYQUIST_WORKER_ID };
        proxy.SetCommand(mInputCmd);
        proxy.mDebug = nyquistSettings.proxyDebug;
        proxy.mControls = move(nyquistSettings.controls);
        auto result = Delegate(proxy, nyquistSettings.proxySettings);
        if (result) {
            mT0 = proxy.mT0;
            mT1 = proxy.mT1;
        }
        return result;
    }

    // Check for reentrant Nyquist commands.
    // I'm choosing to mark skipped Nyquist commands as successful even though
    // they are skipped.  The reason is that when Nyquist calls out to a chain,
    // and that chain contains Nyquist,  it will be clearer if the chain
    // completes skipping Nyquist, rather than doing nothing at all.
    if (mReentryCount > 0) {
        return true;
    }

    // Restore the reentry counter (to zero) when we exit.
    auto countRestorer = valueRestorer(mReentryCount);
    mReentryCount++;
    RegisterFunctions();

    bool success = true;
    int nEffectsSoFar = EffectOutputTracks::nEffectsDone;
    mProjectChanged = false;
    EffectManager& em = EffectManager::Get();
    em.SetSkipStateFlag(false);

    // This code was added in a fix for bug 2392 (no preview for Nyquist)
    // It was commented out in a fix for bug 2428 (no progress dialog from a
    // macro)
    // if (mExternal) {
    //  mProgress->Hide();
    //}

    mOutputTime = 0;
    mCount = 0;
    const auto scale
        =(GetType() == EffectTypeProcess ? 0.5 : 1.0) / GetNumWaveGroups();

    mStop = false;
    mBreak = false;
    mCont = false;

    mTrackIndex = 0;

    // If in tool mode, then we don't do anything with the track and selection.
    const bool bOnePassTool = (GetType() == EffectTypeTool);

    // We must copy all the tracks, because Paste needs label tracks to ensure
    // correct sync-lock group behavior when the timeline is affected; then we
    // just want to operate on the selected wave tracks
    std::optional<EffectOutputTracks> oOutputs;
    if (!bOnePassTool) {
        oOutputs.emplace(
            *mTracks, GetType(), EffectOutputTracks::TimeInterval { mT0, mT1 },
            true, false);
    }

    mNumSelectedChannels
        =bOnePassTool
          ? 0
          : oOutputs->Get().Selected<const WaveTrack>().sum(&WaveTrack::NChannels);

    mDebugOutput = {};
    if (!mHelpFile.empty() && !mHelpFileExists) {
        mDebugOutput
            =XO("error: File \"%s\" specified in header but not found in plug-in path.\n")
              .Format(mHelpFile);
    }

    if (mVersion >= 4) {
        auto project = FindProject();

        mProps = wxEmptyString;

        mProps += wxString::Format(
            wxT("(putprop '*AUDACITY* (list %d %d %d) 'VERSION)\n"),
            AUDACITY_VERSION, AUDACITY_RELEASE, AUDACITY_REVISION);
        wxString lang = gPrefs->Read(wxT("/Locale/Language"), wxT(""));
        lang
            =(lang.empty())
              ? Languages::GetSystemLanguageCode(FileNames::AudacityPathList())
              : lang;
        mProps += wxString::Format(
            wxT("(putprop '*AUDACITY* \"%s\" 'LANGUAGE)\n"), lang);

        mProps += wxString::Format(
            wxT("(setf *DECIMAL-SEPARATOR* #\\%c)\n"),
            wxNumberFormatter::GetDecimalSeparator());

        mProps += wxString::Format(
            wxT("(putprop '*SYSTEM-DIR* \"%s\" 'BASE)\n"),
            EscapeString(FileNames::BaseDir()));
        mProps += wxString::Format(
            wxT("(putprop '*SYSTEM-DIR* \"%s\" 'DATA)\n"),
            EscapeString(FileNames::DataDir()));
        mProps += wxString::Format(
            wxT("(putprop '*SYSTEM-DIR* \"%s\" 'HELP)\n"),
            EscapeString(FileNames::HtmlHelpDir().RemoveLast()));
        mProps += wxString::Format(
            wxT("(putprop '*SYSTEM-DIR* \"%s\" 'TEMP)\n"),
            EscapeString(TempDirectory::TempDir()));
        mProps += wxString::Format(
            wxT("(putprop '*SYSTEM-DIR* \"%s\" 'SYS-TEMP)\n"),
            EscapeString(PlatformCompatibility::GetTempDir()));
        mProps += wxString::Format(
            wxT("(putprop '*SYSTEM-DIR* \"%s\" 'DOCUMENTS)\n"),
            EscapeString(PlatformCompatibility::GetDocumentsDir()));
        mProps += wxString::Format(
            wxT("(putprop '*SYSTEM-DIR* \"%s\" 'HOME)\n"),
            EscapeString(PlatformCompatibility::GetHomeDir()));

        auto paths = NyquistBase::GetNyquistSearchPath();
        wxString list;
        for (size_t i = 0, cnt = paths.size(); i < cnt; i++) {
            list += wxT("\"") + EscapeString(paths[i]) + wxT("\" ");
        }
        list = list.RemoveLast();

        mProps += wxString::Format(
            wxT("(putprop '*SYSTEM-DIR* (list %s) 'PLUGIN)\n"), list);
        mProps += wxString::Format(
            wxT("(putprop '*SYSTEM-DIR* (list %s) 'PLUG-IN)\n"), list);
        mProps += wxString::Format(
            wxT("(putprop '*SYSTEM-DIR* \"%s\" 'USER-PLUG-IN)\n"),
            EscapeString(FileNames::PlugInDir()));

        // Date and time:
        wxDateTime now = wxDateTime::Now();
        int year = now.GetYear();
        int doy = now.GetDayOfYear();
        int dom = now.GetDay();
        // enumerated constants
        wxDateTime::Month month = now.GetMonth();
        wxDateTime::WeekDay day = now.GetWeekDay();

        // Date/time as a list: year, day of year, hour, minute, seconds
        mProps += wxString::Format(
            wxT("(setf *SYSTEM-TIME* (list %d %d %d %d %d))\n"), year, doy,
            now.GetHour(), now.GetMinute(), now.GetSecond());

        mProps += wxString::Format(
            wxT("(putprop '*SYSTEM-TIME* \"%s\" 'DATE)\n"), now.FormatDate());
        mProps += wxString::Format(
            wxT("(putprop '*SYSTEM-TIME* \"%s\" 'TIME)\n"), now.FormatTime());
        mProps += wxString::Format(
            wxT("(putprop '*SYSTEM-TIME* \"%s\" 'ISO-DATE)\n"),
            now.FormatISODate());
        mProps += wxString::Format(
            wxT("(putprop '*SYSTEM-TIME* \"%s\" 'ISO-TIME)\n"),
            now.FormatISOTime());
        mProps
            +=wxString::Format(wxT("(putprop '*SYSTEM-TIME* %d 'YEAR)\n"), year);
        mProps += wxString::Format(
            wxT("(putprop '*SYSTEM-TIME* %d 'DAY)\n"), dom); // day of month
        mProps
            +=wxString::Format(wxT("(putprop '*SYSTEM-TIME* %d 'MONTH)\n"), month);
        mProps += wxString::Format(
            wxT("(putprop '*SYSTEM-TIME* \"%s\" 'MONTH-NAME)\n"),
            now.GetMonthName(month));
        mProps += wxString::Format(
            wxT("(putprop '*SYSTEM-TIME* \"%s\" 'DAY-NAME)\n"),
            now.GetWeekDayName(day));

        mProps += wxString::Format(
            wxT("(putprop '*PROJECT* %d 'PROJECTS)\n"),
            (int)AllProjects {}.size());
        mProps += wxString::Format(
            wxT("(putprop '*PROJECT* \"%s\" 'NAME)\n"),
            EscapeString(project->GetProjectName()));

        int numTracks = 0;
        int numWave = 0;
        int numLabel = 0;
        int numMidi = 0;
        int numTime = 0;
        wxString waveTrackList; // track positions of selected audio tracks.

        {
            auto countRange = TrackList::Get(*project).Any();
            for (auto t : countRange) {
                t->TypeSwitch([&](const WaveTrack&) {
                    numWave++;
                    if (t->GetSelected()) {
                        waveTrackList += wxString::Format(wxT("%d "), 1 + numTracks);
                    }
                });
                numTracks++;
            }
            numLabel = countRange.Filter<const LabelTrack>().size();
#if defined(USE_MIDI)
            numMidi = countRange.Filter<const NoteTrack>().size();
#endif
            numTime = countRange.Filter<const TimeTrack>().size();
        }

        // We use Internat::ToString() rather than "%g" here because we
        // always have to use the dot as decimal separator when giving
        // numbers to Nyquist, whereas using "%g" will use the user's
        // decimal separator which may be a comma in some countries.
        mProps += wxString::Format(
            wxT("(putprop '*PROJECT* (float %s) 'RATE)\n"),
            Internat::ToString(ProjectRate::Get(*project).GetRate()));
        mProps
            +=wxString::Format(wxT("(putprop '*PROJECT* %d 'TRACKS)\n"), numTracks);
        mProps += wxString::Format(
            wxT("(putprop '*PROJECT* %d 'WAVETRACKS)\n"), numWave);
        mProps += wxString::Format(
            wxT("(putprop '*PROJECT* %d 'LABELTRACKS)\n"), numLabel);
        mProps += wxString::Format(
            wxT("(putprop '*PROJECT* %d 'MIDITRACKS)\n"), numMidi);
        mProps += wxString::Format(
            wxT("(putprop '*PROJECT* %d 'TIMETRACKS)\n"), numTime);

        double previewLen = 6.0;
        gPrefs->Read(wxT("/AudioIO/EffectsPreviewLen"), &previewLen);
        mProps += wxString::Format(
            wxT("(putprop '*PROJECT* (float %s) 'PREVIEW-DURATION)\n"),
            Internat::ToString(previewLen));

        // *PREVIEWP* is true when previewing (better than relying on track view).
        wxString isPreviewing = (this->IsPreviewing()) ? wxT("T") : wxT("NIL");
        mProps += wxString::Format(wxT("(setf *PREVIEWP* %s)\n"), isPreviewing);

        mProps += wxString::Format(
            wxT("(putprop '*SELECTION* (list %s) 'TRACKS)\n"), waveTrackList);
        mProps += wxString::Format(
            wxT("(putprop '*SELECTION* %d 'CHANNELS)\n"), mNumSelectedChannels);
    }

    // Nyquist Prompt does not require a selection, but effects do.
    if (!bOnePassTool && (mNumSelectedChannels == 0)) {
        auto message = XO("Audio selection required.");
        using namespace BasicUI;
        BasicUI::ShowMessageBox(
            message, MessageBoxOptions {}.IconStyle(Icon::Error));
    }

    std::optional<TrackIterRange<WaveTrack> > pRange;
    if (!bOnePassTool) {
        pRange.emplace(oOutputs->Get().Selected<WaveTrack>());
    }

    // Keep track of whether the current track is first selected in its sync-lock
    // group (we have no idea what the length of the returned audio will be, so
    // we have to handle sync-lock group behavior the "old" way).
    mFirstInGroup = true;
    Track* gtLast = NULL;
    double progressTot {};

    for (; bOnePassTool || pRange->first != pRange->second;
         (void)(!pRange || (++pRange->first, true))) {
        // Prepare to accumulate more debug output in OutputCallback
        mDebugOutputStr = mDebugOutput.Translation();
        mDebugOutput = Verbatim("%s").Format(std::cref(mDebugOutputStr));

        // New context for each channel group of input
        NyxContext nyxContext {
            [this](double frac) { return TotalProgress(frac); }, scale, progressTot
        };
        auto& mCurNumChannels = nyxContext.mCurNumChannels;
        auto& mCurChannelGroup = nyxContext.mCurChannelGroup;
        auto& mCurTrack = nyxContext.mCurTrack;
        auto& mCurStart = nyxContext.mCurStart;
        auto& mCurLen = nyxContext.mCurLen;

        mCurChannelGroup = pRange ? *pRange->first : nullptr;
        mCurTrack[0] = mCurChannelGroup
                       ? (*mCurChannelGroup->Channels().begin()).get()
                       : nullptr;
        mCurNumChannels = 1;
        assert(mCurChannelGroup != nullptr || bOnePassTool);
        if ((mT1 >= mT0) || bOnePassTool) {
            if (bOnePassTool) {
            } else {
                if (auto channels = mCurChannelGroup->Channels();
                    channels.size() > 1) {
                    // TODO: more-than-two-channels
                    // Pay attention to consistency of mNumSelectedChannels
                    // with the running tally made by this loop!
                    mCurNumChannels = 2;

                    mCurTrack[1] = (*++channels.first).get();
                }

                // Check whether we're in the same group as the last selected track
                Track* gt = *SyncLock::Group(*mCurChannelGroup).first;
                mFirstInGroup = !gtLast || (gtLast != gt);
                gtLast = gt;

                mCurStart = mCurChannelGroup->TimeToLongSamples(mT0);
                auto end = mCurChannelGroup->TimeToLongSamples(mT1);
                mCurLen = end - mCurStart;

                wxASSERT(mCurLen <= NYQ_MAX_LEN);

                mCurLen = std::min(mCurLen, mMaxLen);
            }

            // libnyquist breaks except in LC_NUMERIC=="C".
            //
            // Note that we must set the locale to "C" even before calling
            // nyx_init() because otherwise some effects will not work!
            //
            // MB: setlocale is not thread-safe.  Should use uselocale()
            //     if available, or fix libnyquist to be locale-independent.
            // See also http://bugzilla.audacityteam.org/show_bug.cgi?id=642#c9
            // for further info about this thread safety question.
            wxString prevlocale = wxSetlocale(LC_NUMERIC, NULL);
            wxSetlocale(LC_NUMERIC, wxString(wxT("C")));

            nyx_init();
            nyx_set_os_callback(StaticOSCallback, (void*)this);
            nyx_capture_output(StaticOutputCallback, (void*)this);

            auto cleanup = finally([&] {
                nyx_capture_output(NULL, (void*)NULL);
                nyx_set_os_callback(NULL, (void*)NULL);
                nyx_cleanup();
            });

            if (mVersion >= 4) {
                mPerTrackProps = wxEmptyString;
                wxString lowHz = wxT("nil");
                wxString highHz = wxT("nil");
                wxString centerHz = wxT("nil");
                wxString bandwidth = wxT("nil");

                if (mF0 >= 0.0) {
                    lowHz.Printf(wxT("(float %s)"), Internat::ToString(mF0));
                }

                if (mF1 >= 0.0) {
                    highHz.Printf(wxT("(float %s)"), Internat::ToString(mF1));
                }

                if ((mF0 >= 0.0) && (mF1 >= 0.0)) {
                    centerHz.Printf(
                        wxT("(float %s)"), Internat::ToString(sqrt(mF0 * mF1)));
                }

                if ((mF0 > 0.0) && (mF1 >= mF0)) {
                    // with very small values, bandwidth calculation may be inf.
                    // (Observed on Linux)
                    double bw = log(mF1 / mF0) / log(2.0);
                    if (!std::isinf(bw)) {
                        bandwidth.Printf(wxT("(float %s)"), Internat::ToString(bw));
                    }
                }

                mPerTrackProps += wxString::Format(
                    wxT("(putprop '*SELECTION* %s 'LOW-HZ)\n"), lowHz);
                mPerTrackProps += wxString::Format(
                    wxT("(putprop '*SELECTION* %s 'CENTER-HZ)\n"), centerHz);
                mPerTrackProps += wxString::Format(
                    wxT("(putprop '*SELECTION* %s 'HIGH-HZ)\n"), highHz);
                mPerTrackProps += wxString::Format(
                    wxT("(putprop '*SELECTION* %s 'BANDWIDTH)\n"), bandwidth);

                const auto t0
                    =mCurChannelGroup ? mCurChannelGroup->SnapToSample(mT0) : mT0;
                const auto t1
                    =mCurChannelGroup ? mCurChannelGroup->SnapToSample(mT1) : mT1;
                mPerTrackProps += wxString::Format(
                    wxT("(putprop '*SELECTION* (float %s) 'START)\n"),
                    Internat::ToString(t0));
                mPerTrackProps += wxString::Format(
                    wxT("(putprop '*SELECTION* (float %s) 'END)\n"),
                    Internat::ToString(t1));
            }

            success = ProcessOne(nyxContext, oOutputs ? &*oOutputs : nullptr);

            // Reset previous locale
            wxSetlocale(LC_NUMERIC, prevlocale);

            if (!success || bOnePassTool) {
                goto finish;
            }
            progressTot += nyxContext.mProgressIn + nyxContext.mProgressOut;
        }

        mCount += mCurNumChannels;
    }

    if (mOutputTime > 0.0) {
        mT1 = mT0 + mOutputTime;
    }

finish:

    // Show debug window if trace set in plug-in header and something to show.
    mDebug = (mTrace && !mDebugOutput.Translation().empty()) ? true : mDebug;

    if (mDebug && !mRedirectOutput) {
        ShowDebugOutputHook::Call(mName, mDebugOutput);
    }

    // Has rug been pulled from under us by some effect done within Nyquist??
    if (!bOnePassTool && (nEffectsSoFar == EffectOutputTracks::nEffectsDone)) {
        if (success) {
            oOutputs->Commit();
        }
    } else {
        // Do not use the results.
        // Selection is to be set to whatever it is in the project.
        auto project = FindProject();
        if (project) {
            auto& selectedRegion = ViewInfo::Get(*project).selectedRegion;
            mT0 = selectedRegion.t0();
            mT1 = selectedRegion.t1();
        } else {
            mT0 = 0;
            mT1 = -1;
        }
    }

    if (!mProjectChanged) {
        em.SetSkipStateFlag(true);
    }

    return success;
}

namespace {
wxString GetClipBoundaries(const Track* t)
{
    wxString clips;
    const auto wt = dynamic_cast<const WaveTrack*>(t);
    if (!wt) {
        return clips;
    }
    auto ca = wt->SortedIntervalArray();
    // Each clip is a list (start-time, end-time)
    // Limit number of clips added to avoid argument stack overflow error (bug
    // 2300).
    for (size_t i = 0, n = ca.size(); i < n; ++i) {
        if (i < 1000) {
            clips += wxString::Format(
                wxT("(list (float %s) (float %s))"),
                Internat::ToString(ca[i]->GetPlayStartTime()),
                Internat::ToString(ca[i]->GetPlayEndTime()));
        } else if (i == 1000) {
            // If final clip is NIL, plug-in developer knows there are more than
            // 1000 clips in channel.
            clips += "NIL";
        } else if (i > 1000) {
            break;
        }
    }
    return clips;
}
} // namespace

// NyquistBase implementation

bool NyquistBase::ProcessOne(
    NyxContext& nyxContext, EffectOutputTracks* pOutputs)
{
    const auto mCurNumChannels = nyxContext.mCurNumChannels;
    nyx_rval rval;

    wxString cmd;
    cmd += wxT("(snd-set-latency  0.1)");

    // A tool may be using AUD-DO which will potentially invalidate *TRACK*
    // so tools do not get *TRACK*.
    if (GetType() == EffectTypeTool) {
        cmd += wxT("(setf S 0.25)\n"); // No Track.
    } else if (mVersion >= 4) {
        nyx_set_audio_name("*TRACK*");
        cmd += wxT("(setf S 0.25)\n");
    } else {
        nyx_set_audio_name("S");
        cmd += wxT("(setf *TRACK* '*unbound*)\n");
    }

    if (mVersion >= 4) {
        cmd += mProps;
        cmd += mPerTrackProps;
    }

    const auto& mCurChannelGroup = nyxContext.mCurChannelGroup;

    if ((mVersion >= 4) && (GetType() != EffectTypeTool)) {
        // Set the track TYPE and VIEW properties
        wxString type;
        wxString view;
        wxString bitFormat;
        wxString spectralEditp;

        mCurChannelGroup->TypeSwitch(
            [&](const WaveTrack& wt) {
            type = wxT("wave");
            spectralEditp = SpectrogramSettings::Get(*mCurChannelGroup)
                            .SpectralSelectionEnabled()
                            ? wxT("T")
                            : wxT("NIL");
            view = wxT("NIL");
            // Find() not Get() to avoid creation-on-demand of views in case we
            // are only previewing
            const auto displays = GetDisplaysHook::Call(&wt);
            const auto format = [&](decltype(displays[0]) display) {
                // Get the English name of the view type, without menu codes,
                // as a string that Lisp can examine
                return wxString::Format(
                    wxT("\"%s\""), display.name.Stripped().Debug());
            };
            if (displays.empty()) {
            } else if (displays.size() == 1) {
                view = format(displays[0]);
            } else {
                view = wxT("(list");
                for (auto display : displays) {
                    view += wxString(wxT(" ")) + format(display);
                }
                view += wxT(")");
            }
        },
#if defined(USE_MIDI)
            [&](const NoteTrack&) {
            type = wxT("midi");
            view = wxT("\"Midi\"");
        },
#endif
            [&](const LabelTrack&) {
            type = wxT("label");
            view = wxT("\"Label\"");
        },
            [&](const TimeTrack&) {
            type = wxT("time");
            view = wxT("\"Time\"");
        });

        cmd
            +=wxString::Format(wxT("(putprop '*TRACK* %d 'INDEX)\n"), ++mTrackIndex);
        cmd += wxString::Format(
            wxT("(putprop '*TRACK* \"%s\" 'NAME)\n"),
            EscapeString(mCurChannelGroup->GetName()));
        cmd += wxString::Format(wxT("(putprop '*TRACK* \"%s\" 'TYPE)\n"), type);
        // Note: "View" property may change when Audacity's choice of track views
        // has stabilized.
        cmd += wxString::Format(wxT("(putprop '*TRACK* %s 'VIEW)\n"), view);
        cmd += wxString::Format(
            wxT("(putprop '*TRACK* %d 'CHANNELS)\n"), mCurNumChannels);

        // NOTE: Audacity 2.1.3 True if spectral selection is enabled regardless
        // of track view.
        cmd += wxString::Format(
            wxT("(putprop '*TRACK* %s 'SPECTRAL-EDIT-ENABLED)\n"), spectralEditp);

        const double startTime = mCurChannelGroup->GetStartTime();
        const double endTime = mCurChannelGroup->GetEndTime();

        cmd += wxString::Format(
            wxT("(putprop '*TRACK* (float %s) 'START-TIME)\n"),
            Internat::ToString(startTime));
        cmd += wxString::Format(
            wxT("(putprop '*TRACK* (float %s) 'END-TIME)\n"),
            Internat::ToString(endTime));
        cmd += wxString::Format(
            wxT(
                "(putprop '*TRACK* (float %s) 'GAIN)\n"), // https://github.com/audacity/audacity/issues/7097:
                                                          // not to break all
                                                          // nyquist scripts out
                                                          // there, we keep the old
                                                          // name.
            Internat::ToString(mCurChannelGroup->GetVolume()));
        cmd += wxString::Format(
            wxT("(putprop '*TRACK* (float %s) 'PAN)\n"),
            Internat::ToString(mCurChannelGroup->GetPan()));
        cmd += wxString::Format(
            wxT("(putprop '*TRACK* (float %s) 'RATE)\n"),
            Internat::ToString(mCurChannelGroup->GetRate()));

        switch (mCurChannelGroup->GetSampleFormat()) {
        case int16Sample:
            bitFormat = wxT("16");
            break;
        case int24Sample:
            bitFormat = wxT("24");
            break;
        case floatSample:
            bitFormat = wxT("32.0");
            break;
        }
        cmd
            +=wxString::Format(wxT("(putprop '*TRACK* %s 'FORMAT)\n"), bitFormat);

        float maxPeakLevel = 0.0; // Deprecated as of 2.1.3
        const auto inClipBoundaries = GetClipBoundaries(
            pOutputs ? pOutputs->GetMatchingInput(*mCurChannelGroup) : nullptr);
        const auto outClipBoundaries = GetClipBoundaries(mCurChannelGroup);
        wxString inClips, outClips, peakString, rmsString;
        auto& mCurTrack = nyxContext.mCurTrack;
        for (size_t i = 0; i < mCurNumChannels; i++) {
            float maxPeak = 0.0;
            if (mCurNumChannels > 1) {
                inClips += wxT("(list ");
                outClips += wxT("(list ");
            }
            inClips += inClipBoundaries;
            outClips += outClipBoundaries;
            if (mCurNumChannels > 1) {
                inClips += wxT(" )");
                outClips += wxT(" )");
            }
            float min, max;
            auto pair = WaveChannelUtilities::GetMinMax(
                *mCurTrack[i], mT0, mT1); // may throw
            min = pair.first, max = pair.second;
            maxPeak = std::max(std::max(fabs(min), fabs(max)), maxPeak);
            maxPeakLevel = std::max(maxPeakLevel, maxPeak);

            // On Debian, NaN samples give maxPeak = 3.40282e+38 (FLT_MAX)
            if (
                !std::isinf(maxPeak) && !std::isnan(maxPeak) && (maxPeak < FLT_MAX)) {
                peakString += wxString::Format(
                    wxT("(float %s) "), Internat::ToString(maxPeak));
            } else {
                peakString += wxT("nil ");
            }

            float rms
                =WaveChannelUtilities::GetRMS(*mCurTrack[i], mT0, mT1); // may throw
            if (!std::isinf(rms) && !std::isnan(rms)) {
                rmsString
                    +=wxString::Format(wxT("(float %s) "), Internat::ToString(rms));
            } else {
                rmsString += wxT("NIL ");
            }
        }
        // A list of clips for mono, or an array of lists for multi-channel.
        cmd += wxString::Format(
            wxT("(putprop '*TRACK* %s%s ) 'INCLIPS)\n"),
            (mCurNumChannels == 1) ? wxT("(list ") : wxT("(vector "), inClips);
        cmd += wxString::Format(
            wxT("(putprop '*TRACK* %s%s ) 'CLIPS)\n"),
            (mCurNumChannels == 1) ? wxT("(list ") : wxT("(vector "), outClips);

        (mCurNumChannels > 1)
        ? cmd += wxString::Format(
            wxT("(putprop '*SELECTION* (vector %s) 'PEAK)\n"), peakString)
                 : cmd
                     +=wxString::Format(wxT("(putprop '*SELECTION* %s 'PEAK)\n"), peakString);

        if (
            !std::isinf(maxPeakLevel) && !std::isnan(maxPeakLevel)
            && (maxPeakLevel < FLT_MAX)) {
            cmd += wxString::Format(
                wxT("(putprop '*SELECTION* (float %s) 'PEAK-LEVEL)\n"),
                Internat::ToString(maxPeakLevel));
        }

        (mCurNumChannels > 1)
        ? cmd += wxString::Format(
            wxT("(putprop '*SELECTION* (vector %s) 'RMS)\n"), rmsString)
                 : cmd
                     +=wxString::Format(wxT("(putprop '*SELECTION* %s 'RMS)\n"), rmsString);
    }

    // If in tool mode, then we don't do anything with the track and selection.
    if (GetType() == EffectTypeTool) {
        nyx_set_audio_params(44100, 0);
    } else if (GetType() == EffectTypeGenerate) {
        nyx_set_audio_params(mCurChannelGroup->GetRate(), 0);
    } else {
        auto curLen = nyxContext.mCurLen.as_long_long();
        nyx_set_audio_params(mCurChannelGroup->GetRate(), curLen);
        nyx_set_input_audio(
            NyxContext::StaticGetCallback, &nyxContext, (int)mCurNumChannels,
            curLen, mCurChannelGroup->GetRate());
    }

    // Restore the Nyquist sixteenth note symbol for Generate plug-ins.
    // See http://bugzilla.audacityteam.org/show_bug.cgi?id=490.
    if (GetType() == EffectTypeGenerate) {
        cmd += wxT("(setf s 0.25)\n");
    }

    if (mDebug || mTrace) {
        cmd += wxT("(setf *tracenable* T)\n");
        if (mExternal) {
            cmd += wxT("(setf *breakenable* T)\n");
        }
    } else {
        // Explicitly disable backtrace and prevent values
        // from being carried through to the output.
        // This should be the final command before evaluating the Nyquist script.
        cmd += wxT("(setf *tracenable* NIL)\n");
    }

    for (unsigned int j = 0; j < mControls.size(); j++) {
        if (
            mControls[j].type == NYQ_CTRL_FLOAT
            || mControls[j].type == NYQ_CTRL_FLOAT_TEXT
            || mControls[j].type == NYQ_CTRL_TIME) {
            // We use Internat::ToString() rather than "%f" here because we
            // always have to use the dot as decimal separator when giving
            // numbers to Nyquist, whereas using "%f" will use the user's
            // decimal separator which may be a comma in some countries.
            cmd += wxString::Format(
                wxT("(setf %s %s)\n"), mControls[j].var,
                Internat::ToString(mControls[j].val, 14));
        } else if (
            mControls[j].type == NYQ_CTRL_INT
            || mControls[j].type == NYQ_CTRL_INT_TEXT
            || mControls[j].type == NYQ_CTRL_CHOICE) {
            cmd += wxString::Format(
                wxT("(setf %s %d)\n"), mControls[j].var, (int)(mControls[j].val));
        } else if (
            mControls[j].type == NYQ_CTRL_STRING
            || mControls[j].type == NYQ_CTRL_FILE) {
            cmd += wxT("(setf ");
            // restrict variable names to 7-bit ASCII:
            cmd += mControls[j].var;
            cmd += wxT(" \"");
            cmd += EscapeString(
                mControls[j].valStr); // unrestricted value will become quoted UTF-8
            cmd += wxT("\")\n");
        }
    }

    if (mIsSal) {
        wxString str = EscapeString(mCmd);
        // this is tricky: we need SAL to call main so that we can get a
        // SAL traceback in the event of an error (sal-compile catches the
        // error and calls sal-error-output), but SAL does not return values.
        // We will catch the value in a special global aud:result and if no
        // error occurs, we will grab the value with a LISP expression
        str += wxT("\nset aud:result = main()\n");

        if (mDebug || mTrace) {
            // since we're about to evaluate SAL, remove LISP trace enable and
            // break enable (which stops SAL processing) and turn on SAL stack
            // trace
            cmd += wxT("(setf *tracenable* nil)\n");
            cmd += wxT("(setf *breakenable* nil)\n");
            cmd += wxT("(setf *sal-traceback* t)\n");
        }

        if (mCompiler) {
            cmd += wxT("(setf *sal-compiler-debug* t)\n");
        }

        cmd += wxT("(setf *sal-call-stack* nil)\n");
        // if we do not set this here and an error occurs in main, another
        // error will be raised when we try to return the value of aud:result
        // which is unbound
        cmd += wxT("(setf aud:result nil)\n");
        cmd += wxT("(sal-compile-audacity \"") + str + wxT("\" t t nil)\n");
        // Capture the value returned by main (saved in aud:result), but
        // set aud:result to nil so sound results can be evaluated without
        // retaining audio in memory
        cmd += wxT("(prog1 aud:result (setf aud:result nil))\n");
    } else {
        cmd += mCmd;
    }

    // Evaluate the expression, which may invoke the get callback, but often does
    // not, leaving that to delayed evaluation of the output sound
    rval = nyx_eval_expression(cmd.mb_str(wxConvUTF8));

    // If we're not showing debug window, log errors and warnings:
    const auto output = mDebugOutput.Translation();
    if (!output.empty() && !mDebug && !mTrace) {
        /* i18n-hint: An effect "returned" a message.*/
        wxLogMessage(wxT("\'%s\' returned:\n%s"), mName.Translation(), output);
    }

    // Audacity has no idea how long Nyquist processing will take, but
    // can monitor audio being returned.
    // Anything other than audio should be returned almost instantly
    // so notify the user that process has completed (bug 558)
    if (
        (rval != nyx_audio)
        && ((mCount + mCurNumChannels) == mNumSelectedChannels)) {
        if (mCurNumChannels == 1) {
            TrackProgress(mCount, 1.0, XO("Processing complete."));
        } else {
            TrackGroupProgress(mCount, 1.0, XO("Processing complete."));
        }
    }

    if ((rval == nyx_audio) && (GetType() == EffectTypeTool)) {
        // Catch this first so that we can also handle other errors.
        mDebugOutput
            =/* i18n-hint: Don't translate ';type tool'.  */
              XO("';type tool' effects cannot return audio from Nyquist.\n")
              + mDebugOutput;
        rval = nyx_error;
    }

    if ((rval == nyx_labels) && (GetType() == EffectTypeTool)) {
        // Catch this first so that we can also handle other errors.
        mDebugOutput
            =/* i18n-hint: Don't translate ';type tool'.  */
              XO("';type tool' effects cannot return labels from Nyquist.\n")
              + mDebugOutput;
        rval = nyx_error;
    }

    if (rval == nyx_error) {
        // Return value is not valid type.
        // Show error in debug window if trace enabled, otherwise log.
        if (mTrace) {
            /* i18n-hint: "%s" is replaced by name of plug-in.*/
            mDebugOutput = XO("nyx_error returned from %s.\n")
                           .Format(mName.empty() ? XO("plug-in") : mName)
                           + mDebugOutput;
            mDebug = true;
        } else {
            wxLogMessage(
                "Nyquist returned nyx_error:\n%s", mDebugOutput.Translation());
        }
        return false;
    }

    if (rval == nyx_list) {
        wxLogMessage("Nyquist returned nyx_list");
        if (GetType() == EffectTypeTool) {
            mProjectChanged = true;
        } else {
            BasicUI::ShowMessageBox(XO("Nyquist returned a list."));
        }
        return true;
    }

    if (rval == nyx_string) {
        // Assume the string has already been translated within the Lisp runtime
        // if necessary, by one of the gettext functions defined below, before it
        // is communicated back to C++
        auto msg = Verbatim(NyquistToWxString(nyx_get_string()));
        if (!msg.empty()) { // Empty string may be used as a No-Op return value.
            BasicUI::ShowMessageBox(msg);
        } else if (GetType() == EffectTypeTool) {
            // ;tools may change the project with aud-do commands so
            // it is essential that the state is added to history.
            mProjectChanged = true;
            return true;
        } else {
            // A true no-op.
            return true;
        }

        // True if not process type.
        // If not returning audio from process effect,
        // return first result then stop (disables preview)
        // but allow all output from Nyquist Prompt.
        return GetType() != EffectTypeProcess || mIsPrompt;
    }

    if (rval == nyx_double) {
        auto str = XO("Nyquist returned the value: %f").Format(nyx_get_double());
        BasicUI::ShowMessageBox(str);
        return GetType() != EffectTypeProcess || mIsPrompt;
    }

    if (rval == nyx_int) {
        auto str = XO("Nyquist returned the value: %d").Format(nyx_get_int());
        BasicUI::ShowMessageBox(str);
        return GetType() != EffectTypeProcess || mIsPrompt;
    }

    if (rval == nyx_labels) {
        assert(GetType() != EffectTypeTool); // Guaranteed above
        // Therefore bOnePassTool was false in Process()
        // Therefore output tracks were allocated
        assert(pOutputs);

        mProjectChanged = true;
        unsigned int numLabels = nyx_get_num_labels();
        unsigned int l;
        auto ltrack = *pOutputs->Get().Any<LabelTrack>().begin();
        if (!ltrack) {
            auto newTrack = std::make_shared<LabelTrack>();
            // new track name should be unique among the names in the list of input
            // tracks, not output
            newTrack->SetName(
                inputTracks()->MakeUniqueTrackName(LabelTrack::GetDefaultName()));
            ltrack
                =static_cast<LabelTrack*>(pOutputs->AddToOutputTracks(newTrack));
        }

        for (l = 0; l < numLabels; l++) {
            double t0, t1;
            const char* str;

            // PRL:  to do:
            // let Nyquist analyzers define more complicated selections
            nyx_get_label(l, &t0, &t1, &str);

            ltrack->AddLabel(SelectedRegion(t0 + mT0, t1 + mT0), UTF8CTOWX(str));
        }
        return GetType() != EffectTypeProcess || mIsPrompt;
    }

    wxASSERT(rval == nyx_audio);

    int outChannels = nyx_get_audio_num_channels();
    if (outChannels > (int)mCurNumChannels) {
        BasicUI::ShowMessageBox(
            XO("Nyquist returned too many audio channels.\n"));
        return false;
    }

    if (outChannels == -1) {
        BasicUI::ShowMessageBox(
            XO("Nyquist returned one audio channel as an array.\n"));
        return false;
    }

    if (outChannels == 0) {
        BasicUI::ShowMessageBox(XO("Nyquist returned an empty array.\n"));
        return false;
    }

    nyxContext.mOutputTrack = mCurChannelGroup->EmptyCopy();
    auto out = nyxContext.mOutputTrack;

    // Now fully evaluate the sound
    int success = nyx_get_audio(NyxContext::StaticPutCallback, &nyxContext);

    // See if GetCallback found read errors
    if (auto pException = nyxContext.mpException) {
        std::rethrow_exception(pException);
    }

    if (!success) {
        return false;
    }

    mOutputTime = out->GetEndTime();
    if (mOutputTime <= 0) {
        BasicUI::ShowMessageBox(XO("Nyquist returned nil audio.\n"));
        return false;
    }

    WaveTrack::Holder tempTrack;
    if (outChannels < static_cast<int>(mCurNumChannels)) {
        // Be careful to do this before duplication
        out->Flush();
        // Must destroy one temporary list before repopulating another with
        // correct channel grouping
        nyxContext.mOutputTrack.reset();
        tempTrack = out->MonoToStereo();
    } else {
        tempTrack = move(nyxContext.mOutputTrack);
        out->Flush();
    }

    {
        const bool bMergeClips = (mMergeClips < 0)
                                 // Use sample counts to determine default
                                 // behaviour - times will rarely be equal.
                                 ?
                                 (out->TimeToLongSamples(mT0)
                                  + out->TimeToLongSamples(mOutputTime)
                                  == out->TimeToLongSamples(mT1))
                                 : mMergeClips != 0;
        PasteTimeWarper warper { mT1, mT0 + tempTrack->GetEndTime() };
        mCurChannelGroup->ClearAndPaste(
            mT0, mT1, *tempTrack, mRestoreSplits, bMergeClips, &warper);
    }

    // If we were first in the group adjust non-selected group tracks
    if (mFirstInGroup) {
        for (auto t : SyncLock::Group(*mCurChannelGroup)) {
            if (!t->GetSelected() && SyncLock::IsSyncLockSelected(*t)) {
                t->SyncLockAdjust(mT1, mT0 + out->GetEndTime());
            }
        }

        // Only the first channel can be first in its group
        mFirstInGroup = false;
    }

    mProjectChanged = true;
    return true;
}

// ============================================================================
// NyquistBase Implementation
// ============================================================================

wxString NyquistBase::NyquistToWxString(const char* nyqString)
{
    wxString str(nyqString, wxConvUTF8);
    if (nyqString != NULL && nyqString[0] && str.empty()) {
        // invalid UTF-8 string, convert as Latin-1
        str = _(
            "[Warning: Nyquist returned invalid UTF-8 string, converted here as Latin-1]");
        // TODO: internationalization of strings from Nyquist effects, at least
        // from those shipped with Audacity
        str += LAT1CTOWX(nyqString);
    }
    return str;
}

wxString NyquistBase::EscapeString(const wxString& inStr)
{
    wxString str = inStr;

    str.Replace(wxT("\\"), wxT("\\\\"));
    str.Replace(wxT("\""), wxT("\\\""));

    return str;
}

std::vector<EnumValueSymbol> NyquistBase::ParseChoice(const wxString& text)
{
    std::vector<EnumValueSymbol> results;
    if (text[0] == wxT('(')) {
        // New style:  expecting a Lisp-like list of strings
        Tokenizer tzer;
        tzer.Tokenize(text, true, 1, 1);
        auto& choices = tzer.tokens;
        wxString extra;
        for (auto& choice : choices) {
            auto label = UnQuote(choice, true, &extra);
            if (extra.empty()) {
                results.push_back(TranslatableString { label, {} });
            } else {
                results.push_back({ extra, TranslatableString { label, {} } });
            }
        }
    } else {
        // Old style: expecting a comma-separated list of
        // un-internationalized names, ignoring leading and trailing spaces
        // on each; and the whole may be quoted
        auto choices = wxStringTokenize(
            text[0] == wxT('"') ? text.Mid(1, text.length() - 2) : text, wxT(","));
        for (auto& choice : choices) {
            results.push_back({ choice.Trim(true).Trim(false) });
        }
    }
    return results;
}

FileExtensions NyquistBase::ParseFileExtensions(const wxString& text)
{
    // todo: error handling
    FileExtensions results;
    if (text[0] == wxT('(')) {
        Tokenizer tzer;
        tzer.Tokenize(text, true, 1, 1);
        for (const auto& token : tzer.tokens) {
            results.push_back(UnQuote(token));
        }
    }
    return results;
}

FileNames::FileType NyquistBase::ParseFileType(const wxString& text)
{
    // todo: error handling
    FileNames::FileType result;
    if (text[0] == wxT('(')) {
        Tokenizer tzer;
        tzer.Tokenize(text, true, 1, 1);
        auto& tokens = tzer.tokens;
        if (tokens.size() == 2) {
            result = { UnQuoteMsgid(tokens[0]), ParseFileExtensions(tokens[1]) }
        }
    }
    return result;
}

FileNames::FileTypes NyquistBase::ParseFileTypes(const wxString& text)
{
    // todo: error handling
    FileNames::FileTypes results;
    if (text[0] == wxT('(')) {
        Tokenizer tzer;
        tzer.Tokenize(text, true, 1, 1);
        auto& types = tzer.tokens;
        if (!types.empty() && types[0][0] == wxT('(')) {
            for (auto& type : types) {
                results.push_back(ParseFileType(type));
            }
        }
    }
    if (results.empty()) {
        // Old-style is a specially formatted string, maybe translated
        // Parse it for compatibility
        auto str = UnQuote(text);
        auto pieces = wxSplit(str, '|');
        // Should have an even number
        auto size = pieces.size();
        if (size % 2 == 1) {
            --size, pieces.pop_back();
        }
        for (size_t ii = 0; ii < size; ii += 2) {
            FileExtensions extensions;
            auto extensionStrings = wxSplit(pieces[ii + 1], ';');
            for (const auto& extensionString : extensionStrings) {
                if (extensionString.StartsWith(wxT("*."))) {
                    auto ext = extensionString.substr(2);
                    if (ext == wxT("*")) {
                        // "*.*" to match all
                        ext.clear();
                    }
                    extensions.push_back(ext);
                }
            }
            results.push_back({ Verbatim(pieces[ii]), extensions });
        }
    }
    return results;
}

void NyquistBase::RedirectOutput()
{
    mRedirectOutput = true;
}

void NyquistBase::SetCommand(const wxString& cmd)
{
    mExternal = true;

    if (cmd.size()) {
        ParseCommand(cmd);
    }
}

void NyquistBase::Break()
{
    mBreak = true;
}

void NyquistBase::Continue()
{
    mCont = true;
}

void NyquistBase::Stop()
{
    mStop = true;
}

TranslatableString NyquistBase::UnQuoteMsgid(
    const wxString& s, bool allowParens, wxString* pExtraString)
{
    if (pExtraString) {
        *pExtraString = wxString {}
    }

    int len = s.length();
    if (len >= 2 && s[0] == wxT('\"') && s[len - 1] == wxT('\"')) {
        auto unquoted = s.Mid(1, len - 2);
        // Sorry, no context strings, yet
        // (See also comments in NyquistEffectsModule::AutoRegisterPlugins)
        return TranslatableString { unquoted, {} };
    } else if (
        allowParens && len >= 2 && s[0] == wxT('(') && s[len - 1] == wxT(')')) {
        Tokenizer tzer;
        tzer.Tokenize(s, true, 1, 1);
        auto& tokens = tzer.tokens;
        if (tokens.size() > 1) {
            if (pExtraString && tokens[1][0] == '(') {
                // A choice with a distinct internal string form like
                // ("InternalString" (_ "Visible string"))
                // Recur to find the two strings
                *pExtraString = UnQuote(tokens[0], false);
                return UnQuoteMsgid(tokens[1]);
            } else {
                // Assume the first token was _ -- we don't check that
                // And the second is the string, which is internationalized
                // Sorry, no context strings, yet
                return UnQuoteMsgid(tokens[1], false);
            }
        } else {
            return {}
        }
    } else {
        // If string was not quoted, assume no translation exists
        return Verbatim(s);
    }
}

wxString NyquistBase::UnQuote(
    const wxString& s, bool allowParens, wxString* pExtraString)
{
    return UnQuoteMsgid(s, allowParens, pExtraString).Translation();
}

double NyquistBase::GetCtrlValue(const wxString& s)
{
    /* For this to work correctly requires that the plug-in header is
     * parsed on each run so that the correct value for "half-srate" may
     * be determined.
     *
    auto project = FindProject();
    if (project && s.IsSameAs(wxT("half-srate"), false)) {
       auto rate =
          TrackList::Get( *project ).Selected< const WaveTrack >()
             .min( &WaveTrack::GetRate );
       return (rate / 2.0);
    }
    */

    return Internat::CompatibleToDouble(s);
}

bool NyquistBase::Tokenizer::Tokenize(
    const wxString& line, bool eof, size_t trimStart, size_t trimEnd)
{
    auto endToken = [&] {
        if (!tok.empty()) {
            tokens.push_back(tok);
            tok = wxT("");
        }
    };

    for (auto c :
         make_iterator_range(line.begin() + trimStart, line.end() - trimEnd)) {
        if (q && !sl && c == wxT('\\')) {
            // begin escaped character, only within quotes
            sl = true;
            continue;
        }

        if (!sl && c == wxT('"')) {
            // Unescaped quote
            if (!q) {
                // start of string
                if (!paren) {
                    // finish previous token
                    endToken();
                }
                // Include the delimiter in the token
                tok += c;
                q = true;
            } else {
                // end of string
                // Include the delimiter in the token
                tok += c;
                if (!paren) {
                    endToken();
                }
                q = false;
            }
        } else if (!q && !paren && (c == wxT(' ') || c == wxT('\t'))) {
            // Unenclosed whitespace
            // Separate tokens; don't accumulate this character
            endToken();
        } else if (!q && c == wxT(';')) {
            // semicolon not in quotes, but maybe in parentheses
            // Lisp style comments with ; (but not with #| ... |#) are allowed
            // within a wrapped header multi-line, so that i18n hint comments may
            // be placed before strings and found by xgettext
            break;
        } else if (!q && c == wxT('(')) {
            // Start of list or sublist
            if (++paren == 1) {
                // finish previous token; begin list, including the delimiter
                endToken(), tok += c;
            } else {
                // defer tokenizing of nested list to a later pass over the token
                tok += c;
            }
        } else if (!q && c == wxT(')')) {
            // End of list or sublist
            if (--paren == 0) {
                // finish list, including the delimiter
                tok += c, endToken();
            } else if (paren < 0) {
                // forgive unbalanced right paren
                paren = 0, endToken();
            } else {
                // nested list; deferred tokenizing
                tok += c;
            }
        } else {
            if (sl && paren) {
                // Escaped character in string inside list, to be parsed again
                // Put the escape back for the next pass
                tok += wxT('\\');
            }
            if (sl && !paren && c == 'n') {
                // Convert \n to newline, the only special escape besides \\ or \"
                // But this should not be used if a string needs to localize.
                // Instead, simply put a line break in the string.
                c = '\n';
            }
            tok += c;
        }

        sl = false;
    }

    if (eof || (!q && !paren)) {
        endToken();
        return true;
    } else {
        // End of line but not of file, and a string or list is yet unclosed
        // If a string, accumulate a newline character
        if (q) {
            tok += wxT('\n');
        }
        return false;
    }
}

bool NyquistBase::Parse(
    Tokenizer& tzer, const wxString& line, bool eof, bool first)
{
    if (!tzer.Tokenize(line, eof, first ? 1 : 0, 0)) {
        return false;
    }

    const auto& tokens = tzer.tokens;
    int len = tokens.size();
    if (len < 1) {
        return true;
    }

    // Consistency decision is for "plug-in" as the correct spelling
    // "plugin" (deprecated) is allowed as an undocumented convenience.
    if (
        len == 2 && tokens[0] == wxT("nyquist")
        && (tokens[1] == wxT("plug-in") || tokens[1] == wxT("plugin"))) {
        mOK = true;
        return true;
    }

    if (len >= 2 && tokens[0] == wxT("type")) {
        wxString tok = tokens[1];
        mIsTool = false;
        if (tok == wxT("tool")) {
            mIsTool = true;
            mType = EffectTypeTool;
            // we allow
            // ;type tool
            // ;type tool process
            // ;type tool generate
            // ;type tool analyze
            // The last three are placed in the tool menu, but are processed as
            // process, generate or analyze.
            if (len >= 3) {
                tok = tokens[2];
            }
        }

        if (tok == wxT("process")) {
            mType = EffectTypeProcess;
        } else if (tok == wxT("generate")) {
            mType = EffectTypeGenerate;
        } else if (tok == wxT("analyze")) {
            mType = EffectTypeAnalyze;
        }

        if (len >= 3 && tokens[2] == wxT("spectral")) {
            mIsSpectral = true;
        }
        return true;
    }

    if (len == 2 && tokens[0] == wxT("codetype")) {
        // This will stop ParseProgram() from doing a best guess as program type.
        if (tokens[1] == wxT("lisp")) {
            mIsSal = false;
            mFoundType = true;
        } else if (tokens[1] == wxT("sal")) {
            mIsSal = true;
            mFoundType = true;
        }
        return true;
    }

    if (len >= 2 && tokens[0] == wxT("debugflags")) {
        for (int i = 1; i < len; i++) {
            // "trace" sets *tracenable* (LISP) or *sal-traceback* (SAL)
            // and displays debug window IF there is anything to show.
            if (tokens[i] == wxT("trace")) {
                mTrace = true;
            } else if (tokens[i] == wxT("notrace")) {
                mTrace = false;
            } else if (tokens[i] == wxT("compiler")) {
                mCompiler = true;
            } else if (tokens[i] == wxT("nocompiler")) {
                mCompiler = false;
            }
        }
        return true;
    }

    // We support versions 1, 2 and 3
    // (Version 2 added support for string parameters.)
    // (Version 3 added support for choice parameters.)
    // (Version 4 added support for project/track/selection information.)
    if (len >= 2 && tokens[0] == wxT("version")) {
        long v;
        tokens[1].ToLong(&v);
        if (v < 1 || v > 4) {
            // This is an unsupported plug-in version
            mOK = false;
            mInitError
                =XO("This version of Audacity does not support Nyquist plug-in version %ld")
                  .Format(v);
            return true;
        }
        mVersion = (int)v;
    }

    if (len >= 2 && tokens[0] == wxT("name")) {
        // Names do not yet support context strings for translations, or
        // internal names distinct from visible English names.
        // (See also comments in NyquistEffectsModule::AutoRegisterPlugins)
        auto name = UnQuote(tokens[1]);
        // Strip ... from name if it's present, perhaps in third party plug-ins
        // Menu system puts ... back if there are any controls
        // This redundant naming convention must NOT be followed for
        // shipped Nyquist effects with internationalization.  Else the msgid
        // later looked up will lack the ... and will not be found.
        if (name.EndsWith(wxT("..."))) {
            name = name.RemoveLast(3);
        }
        mName = TranslatableString { name, {} };
        return true;
    }

    if (len >= 2 && tokens[0] == wxT("action")) {
        mAction = TranslatableString { UnQuote(tokens[1]), {} };
        return true;
    }

    if (len >= 2 && tokens[0] == wxT("info")) {
        mInfo = TranslatableString { UnQuote(tokens[1]), {} };
        return true;
    }

    if (len >= 2 && tokens[0] == wxT("preview")) {
        if (tokens[1] == wxT("enabled") || tokens[1] == wxT("true")) {
            mEnablePreview = true;
            SetLinearEffectFlag(false);
        } else if (tokens[1] == wxT("linear")) {
            mEnablePreview = true;
            SetLinearEffectFlag(true);
        } else if (tokens[1] == wxT("selection")) {
            mEnablePreview = true;
            SetPreviewFullSelectionFlag(true);
        } else if (tokens[1] == wxT("disabled") || tokens[1] == wxT("false")) {
            mEnablePreview = false;
        }
        return true;
    }

    // Maximum number of samples to be processed. This can help the
    // progress bar if effect does not process all of selection.
    if (len >= 2 && tokens[0] == wxT("maxlen")) {
        long long v; // Note that Nyquist may overflow at > 2^31 samples (bug 439)
        tokens[1].ToLongLong(&v);
        mMaxLen = (sampleCount)v;
    }

    if (len >= 2 && tokens[0] == wxT("mergeclips")) {
        long v;
        // -1 = auto (default), 0 = don't merge clips, 1 = do merge clips
        tokens[1].ToLong(&v);
        mMergeClips = v;
        return true;
    }

    if (len >= 2 && tokens[0] == wxT("restoresplits")) {
        long v;
        // Splits are restored by default. Set to 0 to prevent.
        tokens[1].ToLong(&v);
        mRestoreSplits = !!v;
        return true;
    }

    if (len >= 2 && tokens[0] == wxT("author")) {
        mAuthor = TranslatableString { UnQuote(tokens[1]), {} };
        return true;
    }

    if (len >= 2 && tokens[0] == wxT("release")) {
        // Value must be quoted if the release version string contains spaces.
        mReleaseVersion = TranslatableString { UnQuote(tokens[1]), {} };
        return true;
    }

    if (len >= 2 && tokens[0] == wxT("copyright")) {
        mCopyright = TranslatableString { UnQuote(tokens[1]), {} };
        return true;
    }

    // Page name in Audacity development manual
    if (len >= 2 && tokens[0] == wxT("manpage")) {
        // do not translate
        mManPage = UnQuote(tokens[1], false);
        return true;
    }

    // Local Help file
    if (len >= 2 && tokens[0] == wxT("helpfile")) {
        // do not translate
        mHelpFile = UnQuote(tokens[1], false);
        return true;
    }

    // Debug button may be disabled for release plug-ins.
    if (len >= 2 && tokens[0] == wxT("debugbutton")) {
        if (tokens[1] == wxT("disabled") || tokens[1] == wxT("false")) {
            mDebugButton = false;
        }
        return true;
    }

    if (len >= 3 && tokens[0] == wxT("control")) {
        NyqControl ctrl;

        if (len == 3 && tokens[1] == wxT("text")) {
            ctrl.var = tokens[1];
            ctrl.label = UnQuote(tokens[2]);
            ctrl.type = NYQ_CTRL_TEXT;
        } else if (len >= 5) {
            ctrl.var = tokens[1];
            ctrl.name = UnQuote(tokens[2]);
            // 3 is type, below
            ctrl.label = tokens[4];

            // valStr may or may not be a quoted string
            ctrl.valStr = len > 5 ? tokens[5] : wxString {};
            ctrl.val = GetCtrlValue(ctrl.valStr);
            if (
                ctrl.valStr.length() > 0
                && (ctrl.valStr[0] == wxT('(') || ctrl.valStr[0] == wxT('"'))) {
                ctrl.valStr = UnQuote(ctrl.valStr);
            }

            // 6 is minimum, below
            // 7 is maximum, below

            if (tokens[3] == wxT("string")) {
                ctrl.type = NYQ_CTRL_STRING;
                ctrl.label = UnQuote(ctrl.label);
            } else if (tokens[3] == wxT("choice")) {
                ctrl.type = NYQ_CTRL_CHOICE;
                ctrl.choices = ParseChoice(ctrl.label);
                ctrl.label = wxT("");
            } else if (tokens[3] == wxT("file")) {
                ctrl.type = NYQ_CTRL_FILE;
                ctrl.fileTypes = ParseFileTypes(tokens[6]);
                // will determine file dialog styles:
                ctrl.highStr = UnQuote(tokens[7]);
                ctrl.label = UnQuote(ctrl.label);
            } else {
                ctrl.label = UnQuote(ctrl.label);

                if (len < 8) {
                    return true;
                }

                if (
                    (tokens[3] == wxT("float"))
                    || (tokens[3] == wxT("real"))) { // Deprecated
                    ctrl.type = NYQ_CTRL_FLOAT;
                } else if (tokens[3] == wxT("int")) {
                    ctrl.type = NYQ_CTRL_INT;
                } else if (tokens[3] == wxT("float-text")) {
                    ctrl.type = NYQ_CTRL_FLOAT_TEXT;
                } else if (tokens[3] == wxT("int-text")) {
                    ctrl.type = NYQ_CTRL_INT_TEXT;
                } else if (tokens[3] == wxT("time")) {
                    ctrl.type = NYQ_CTRL_TIME;
                } else {
                    wxString str;
                    str.Printf(
                        wxT(
                            "Bad Nyquist 'control' type specification: '%s' in plug-in file '%s'.\nControl not created."),
                        tokens[3], mFileName.GetFullPath());

                    // Too disturbing to show alert before Audacity frame is up.
                    //    EffectUIServices::DoMessageBox(*this,
                    //       str,
                    //       wxOK | wxICON_EXCLAMATION,
                    //       XO("Nyquist Warning") );

                    // Note that the AudacityApp's mLogger has not yet been created,
                    // so this brings up an alert box, but after the Audacity frame
                    // is up.
                    wxLogWarning(str);
                    return true;
                }

                ctrl.lowStr = UnQuote(tokens[6]);
                if (
                    ctrl.type == NYQ_CTRL_INT_TEXT
                    && ctrl.lowStr.IsSameAs(wxT("nil"), false)) {
                    ctrl.low = INT_MIN;
                } else if (
                    ctrl.type == NYQ_CTRL_FLOAT_TEXT
                    && ctrl.lowStr.IsSameAs(wxT("nil"), false)) {
                    ctrl.low = -(FLT_MAX);
                } else if (
                    ctrl.type == NYQ_CTRL_TIME
                    && ctrl.lowStr.IsSameAs(wxT("nil"), false)) {
                    ctrl.low = 0.0;
                } else {
                    ctrl.low = GetCtrlValue(ctrl.lowStr);
                }

                ctrl.highStr = UnQuote(tokens[7]);
                if (
                    ctrl.type == NYQ_CTRL_INT_TEXT
                    && ctrl.highStr.IsSameAs(wxT("nil"), false)) {
                    ctrl.high = INT_MAX;
                } else if (
                    (ctrl.type == NYQ_CTRL_FLOAT_TEXT
                     || ctrl.type == NYQ_CTRL_TIME)
                    && ctrl.highStr.IsSameAs(wxT("nil"), false)) {
                    ctrl.high = FLT_MAX;
                } else {
                    ctrl.high = GetCtrlValue(ctrl.highStr);
                }

                if (ctrl.high < ctrl.low) {
                    ctrl.high = ctrl.low;
                }

                if (ctrl.val < ctrl.low) {
                    ctrl.val = ctrl.low;
                }

                if (ctrl.val > ctrl.high) {
                    ctrl.val = ctrl.high;
                }

                ctrl.ticks = 1000;
                if (
                    ctrl.type == NYQ_CTRL_INT && (ctrl.high - ctrl.low < ctrl.ticks)) {
                    ctrl.ticks = (int)(ctrl.high - ctrl.low);
                }
            }
        }

        if (!make_iterator_range(mPresetNames).contains(ctrl.var)) {
            mControls.push_back(ctrl);
        }
    }

    // Deprecated
    if (len >= 2 && tokens[0] == wxT("categories")) {
        for (size_t i = 1; i < tokens.size(); ++i) {
            mCategories.push_back(tokens[i]);
        }
    }
    return true;
}

bool NyquistBase::ParseProgram(wxInputStream& stream)
{
    if (!stream.IsOk()) {
        mInitError = XO("Could not open file");
        return false;
    }

    wxTextInputStream pgm(stream, wxT(" \t"), wxConvAuto());

    mCmd = wxT("");
    mCmd.Alloc(10000);
    mIsSal = false;
    mControls.clear();
    mCategories.clear();
    mIsSpectral = false;
    mManPage = wxEmptyString; // If not wxEmptyString, must be a page in the
                              // Audacity manual.
    mHelpFile
        =wxEmptyString; // If not wxEmptyString, must be a valid HTML help file.
    mHelpFileExists = false;
    mDebug = false;
    mTrace = false;
    mDebugButton = true;  // Debug button enabled by default.
    mEnablePreview = true; // Preview button enabled by default.

    // Bug 1934.
    // All Nyquist plug-ins should have a ';type' field, but if they don't we
    // default to being an Effect.
    mType = EffectTypeProcess;

    mFoundType = false;
    while (!stream.Eof() && stream.IsOk())
    {
        wxString line = pgm.ReadLine();
        if (
            line.length() > 1
            &&// New in 2.3.0:  allow magic comment lines to start with $
              // The trick is that xgettext will not consider such lines comments
              // and will extract the strings they contain
            (line[0] == wxT(';') || line[0] == wxT('$'))) {
            Tokenizer tzer;
            unsigned nLines = 1;
            bool done;
            // Allow continuations within control lines.
            bool control = line[0] == wxT('$') || line.StartsWith(wxT(";control"));
            do{
                done = Parse(tzer, line, !control || stream.Eof(), nLines == 1);
            }while (!done && (line = pgm.ReadLine(), ++nLines, true));

            // Don't pass these lines to the interpreter, so it doesn't get
            // confused by $, but pass blanks, so that SAL effects compile with
            // proper line numbers
            while (nLines--) {
                mCmd += wxT('\n');
            }
        } else {
            if (!mFoundType && line.length() > 0) {
                if (
                    line[0] == wxT('(')
                    || (line[0] == wxT('#') && line.length() > 1
                        && line[1] == wxT('|'))) {
                    mIsSal = false;
                    mFoundType = true;
                } else if (line.Upper().Find(wxT("RETURN")) != wxNOT_FOUND) {
                    mIsSal = true;
                    mFoundType = true;
                }
            }
            mCmd += line + wxT("\n");
        }
    }
    if (!mFoundType && mIsPrompt) {
        using namespace BasicUI;
        /* i1n-hint: SAL and LISP are names for variant syntaxes for the
         Nyquist programming language.  Leave them, and 'return', untranslated. */
        BasicUI::ShowMessageBox(
            XO(
                "Your code looks like SAL syntax, but there is no \'return\' statement.\n\
For SAL, use a return statement such as:\n\treturn *track* * 0.1\n\
or for LISP, begin with an open parenthesis such as:\n\t(mult *track* 0.1)\n ."),
            MessageBoxOptions {}.IconStyle(Icon::Error));
        /* i18n-hint: refers to programming "languages" */
        mInitError = XO("Could not determine language");
        return false;
        // Else just throw it at Nyquist to see what happens
    }

    const auto helpStuff = CheckHelpPage();
    mHelpFileExists = helpStuff.first;
    mHelpPage = helpStuff.second;

    return true;
}

void NyquistBase::ParseFile()
{
    wxFileInputStream rawStream(mFileName.GetFullPath());
    wxBufferedInputStream stream(rawStream, 10000);

    ParseProgram(stream);
}

bool NyquistBase::ParseCommand(const wxString& cmd)
{
    wxStringInputStream stream(cmd + wxT(" "));

    return ParseProgram(stream);
}

int NyquistBase::NyxContext::StaticGetCallback(
    float* buffer, int channel, int64_t start, int64_t len, int64_t totlen,
    void* userdata)
{
    auto This = static_cast<NyxContext*>(userdata);
    return This->GetCallback(buffer, channel, start, len, totlen);
}

int NyquistBase::NyxContext::GetCallback(
    float* buffer, int ch, int64_t start, int64_t len, int64_t)
{
    if (mCurBuffer[ch]) {
        if (
            (mCurStart + start) < mCurBufferStart[ch]
            || (mCurStart + start) + len > mCurBufferStart[ch] + mCurBufferLen[ch]) {
            mCurBuffer[ch].reset();
        }
    }

    if (!mCurBuffer[ch]) {
        mCurBufferStart[ch] = (mCurStart + start);
        mCurBufferLen[ch] = mCurTrack[ch]->GetBestBlockSize(mCurBufferStart[ch]);

        if (mCurBufferLen[ch] < (size_t)len) {
            mCurBufferLen[ch] = mCurTrack[ch]->GetIdealBlockSize();
        }

        mCurBufferLen[ch] = limitSampleBufferSize(
            mCurBufferLen[ch], mCurStart + mCurLen - mCurBufferStart[ch]);

        // C++20
        // mCurBuffer[ch] = std::make_unique_for_overwrite(mCurBufferLen[ch]);
        mCurBuffer[ch] = Buffer { safenew float[mCurBufferLen[ch]] };
        try
        {
            mCurTrack[ch]->GetFloats(
                mCurBuffer[ch].get(), mCurBufferStart[ch], mCurBufferLen[ch]);
        }
        catch (...)
        {
            // Save the exception object for re-throw when out of the library
            mpException = std::current_exception();
            return -1;
        }
    }

    // We have guaranteed above that this is nonnegative and bounded by
    // mCurBufferLen[ch]:
    auto offset = (mCurStart + start - mCurBufferStart[ch]).as_size_t();
    const void* src = &mCurBuffer[ch][offset];
    std::memcpy(buffer, src, len * sizeof(float));

    if (ch == 0) {
        double progress = mScale * ((start + len) / mCurLen.as_double());
        if (progress > mProgressIn) {
            mProgressIn = progress;
        }
        if (mProgressReport(mProgressIn + mProgressOut + mProgressTot)) {
            return -1;
        }
    }

    return 0;
}

int NyquistBase::NyxContext::StaticPutCallback(
    float* buffer, int channel, int64_t start, int64_t len, int64_t totlen,
    void* userdata)
{
    auto This = static_cast<NyxContext*>(userdata);
    return This->PutCallback(buffer, channel, start, len, totlen);
}

int NyquistBase::NyxContext::PutCallback(
    float* buffer, int channel, int64_t start, int64_t len, int64_t totlen)
{
    // Don't let C++ exceptions propagate through the Nyquist library
    return GuardedCall<int>(
        [&] {
        if (channel == 0) {
            double progress = mScale * ((float)(start + len) / totlen);
            if (progress > mProgressOut) {
                mProgressOut = progress;
            }
            if (mProgressReport(mProgressIn + mProgressOut + mProgressTot)) {
                return -1;
            }
        }

        auto iChannel = mOutputTrack->Channels().begin();
        std::advance(iChannel, channel);
        const auto pChannel = *iChannel;
        pChannel->Append((samplePtr)buffer, floatSample, len);

        return 0;  // success
    },
        MakeSimpleGuard(-1)); // translate all exceptions into failure
}

void NyquistBase::StaticOutputCallback(int c, void* This)
{
    ((NyquistBase*)This)->OutputCallback(c);
}

void NyquistBase::OutputCallback(int c)
{
    // Always collect Nyquist error messages for normal plug-ins
    if (!mRedirectOutput) {
        mDebugOutputStr += (wxChar)c;
        return;
    }

    std::cout << (char)c;
}

void NyquistBase::StaticOSCallback(void* This)
{
    ((NyquistBase*)This)->OSCallback();
}

void NyquistBase::OSCallback()
{
    if (mStop) {
        mStop = false;
        nyx_stop();
    } else if (mBreak) {
        mBreak = false;
        nyx_break();
    } else if (mCont) {
        mCont = false;
        nyx_continue();
    }

    YieldIfNeededHook::Call();
}

FilePaths NyquistBase::GetNyquistSearchPath()
{
    const auto& audacityPathList = FileNames::AudacityPathList();
    FilePaths pathList;

    for (size_t i = 0; i < audacityPathList.size(); i++) {
        wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
        FileNames::AddUniquePathToPathList(prefix + wxT("nyquist"), pathList);
        FileNames::AddUniquePathToPathList(prefix + wxT("plugins"), pathList);
        FileNames::AddUniquePathToPathList(prefix + wxT("plug-ins"), pathList);
    }
    pathList.push_back(FileNames::PlugInDir());

    return pathList;
}

bool NyquistBase::IsOk()
{
    return mOK;
}

/*!
 A file path given to Nyquist may be a platform-independent canonicalized
 form using certain abbreviations that are expanded into the platform-dependent
 equivalent.

 If the path names only a directory, also append "/untitled" plus extension
 */
void NyquistBase::resolveFilePath(
    wxString& path, FileExtension extension /* empty string */)
{
#if defined(__WXMSW__)
    path.Replace("/", wxFileName::GetPathSeparator());
#endif

    path.Trim(true).Trim(false);

    typedef std::unordered_map<wxString, FilePath> map;
    map pathKeys = {
        { "*home*", PlatformCompatibility::GetHomeDir() },
        { "~", PlatformCompatibility::GetHomeDir() },
        { "*default*", FileNames::DefaultToDocumentsFolder("").GetPath() },
        { "*export*", FileNames::FindDefaultPath(FileNames::Operation::Export) },
        { "*save*", FileNames::FindDefaultPath(FileNames::Operation::Save) },
        { "*config*", FileNames::DataDir() }
    };

    int characters = path.Find(wxFileName::GetPathSeparator());
    if (characters == wxNOT_FOUND) { // Just a path or just a file name
        if (path.empty()) {
            path = "*default*";
        }

        if (pathKeys.find(path) != pathKeys.end()) {
            // Keyword found, so assume this is the intended directory.
            path = pathKeys[path] + wxFileName::GetPathSeparator();
        } else { // Just a file name
            path = pathKeys["*default*"] + wxFileName::GetPathSeparator() + path;
        }
    } else { // path + file name
        wxString firstDir = path.Left(characters);
        wxString rest = path.Mid(characters);

        if (pathKeys.find(firstDir) != pathKeys.end()) {
            path = pathKeys[firstDir] + rest;
        }
    }

    wxFileName fname = path;

    // If the directory is invalid, better to leave it as is (invalid) so that
    // the user sees the error rather than an unexpected file path.
    if (fname.wxFileName::IsOk() && fname.GetFullName().empty()) {
        path = fname.GetPathWithSep() + _("untitled");
        if (!extension.empty()) {
            path = path + '.' + extension;
        }
    }
}

bool NyquistBase::validatePath(wxString path)
{
    wxFileName fname = path;
    wxString dir = fname.GetPath();

    return
        fname.wxFileName::IsOk() && wxFileName::DirExists(dir)
        && !fname.GetFullName().empty();
}

wxString NyquistBase::ToTimeFormat(double t)
{
    int seconds = static_cast<int>(t);
    int hh = seconds / 3600;
    int mm = seconds % 3600;
    mm = mm / 60;
    return wxString::Format("%d:%d:%.3f", hh, mm, t - (hh * 3600 + mm * 60));
}

static LVAL gettext()
{
    auto string = UTF8CTOWX(getstring(xlgastring()));
#if !HAS_I18N_CONTEXTS
    // allow ignored context argument
    if (moreargs()) {
        nextarg();
    }
#endif
    xllastarg();
    return cvstring(GetCustomTranslation(string).mb_str(wxConvUTF8));
}

static LVAL gettextc()
{
#if HAS_I18N_CONTEXTS
    auto string = UTF8CTOWX(getstring(xlgastring()));
    auto context = UTF8CTOWX(getstring(xlgastring()));
    xllastarg();
    return cvstring(
        wxGetTranslation(string, "", 0, "", context).mb_str(wxConvUTF8));
#else
    return gettext();
#endif
}

static LVAL ngettext()
{
    auto string1 = UTF8CTOWX(getstring(xlgastring()));
    auto string2 = UTF8CTOWX(getstring(xlgastring()));
    auto number = getfixnum(xlgafixnum());
#if !HAS_I18N_CONTEXTS
    // allow ignored context argument
    if (moreargs()) {
        nextarg();
    }
#endif
    xllastarg();
    return cvstring(
        wxGetTranslation(string1, string2, number).mb_str(wxConvUTF8));
}

static LVAL ngettextc()
{
#if HAS_I18N_CONTEXTS
    auto string1 = UTF8CTOWX(getstring(xlgastring()));
    auto string2 = UTF8CTOWX(getstring(xlgastring()));
    auto number = getfixnum(xlgafixnum());
    auto context = UTF8CTOWX(getstring(xlgastring()));
    xllastarg();
    return cvstring(wxGetTranslation(string1, string2, number, "", context)
                    .mb_str(wxConvUTF8));
#else
    return ngettext();
#endif
}

void* nyq_make_opaque_string(int size, unsigned char* src)
{
    LVAL dst;
    unsigned char* dstp;
    dst = new_string((int)(size + 2));
    dstp = getstring(dst);

    /* copy the source to the destination */
    while (size-- > 0) {
        *dstp++ = *src++;
    }
    *dstp = '\0';

    return (void*)dst;
}

void* nyq_reformat_aud_do_response(const wxString& Str)
{
    LVAL dst;
    LVAL message;
    LVAL success;
    wxString Left = Str.BeforeLast('\n').BeforeLast('\n').ToAscii();
    wxString Right = Str.BeforeLast('\n').AfterLast('\n').ToAscii();
    message = cvstring(Left);
    success = Right.EndsWith("OK") ? s_true : nullptr;
    dst = cons(message, success);
    return (void*)dst;
}

void* ExecForLisp(char* pIn)
{
    wxString Str1(pIn);
    wxString Str2;

    NyquistBase::ExecFromMainHook::Call(&Str1, &Str2);

    return nyq_reformat_aud_do_response(Str2);
}

/* xlc_aud_do -- interface to C routine aud_do */
/**/
LVAL xlc_aud_do(void)
{
    // Based on string-trim...
    unsigned char* leftp;
    LVAL src, dst;

    /* get the string */
    src = xlgastring();
    xllastarg();

    /* setup the string pointer */
    leftp = getstring(src);

    // Go call my real function here...
    dst = (LVAL)ExecForLisp((char*)leftp);

    // dst = cons(dst, (LVAL)1);
    /* return the new string */
    return dst;
}

static void RegisterFunctions()
{
    // Add functions to XLisp.  Do this only once,
    // before the first call to nyx_init.
    static bool firstTime = true;
    if (firstTime) {
        firstTime = false;

        // All function names must be UP-CASED
        static const FUNDEF functions[] = {
            { "_", SUBR, gettext },         { "_C", SUBR, gettextc },
            { "NGETTEXT", SUBR, ngettext }, { "NGETTEXTC", SUBR, ngettextc },
            { "AUD-DO", SUBR, xlc_aud_do },
        };

        xlbindfunctions(functions, WXSIZEOF(functions));
    }
}
