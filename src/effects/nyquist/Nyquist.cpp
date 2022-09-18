/**********************************************************************

  Audacity: A Digital Audio Editor

  Nyquist.cpp

  Dominic Mazzoni

******************************************************************//**

\class NyquistEffect
\brief An Effect that calls up a Nyquist (XLISP) plug-in, i.e. many possible
effects from this one class.

*//****************************************************************//**

\class NyquistOutputDialog
\brief Dialog used with NyquistEffect

*//*******************************************************************/


#include "Nyquist.h"
#include "NyquistParser.h"
#include "nyx.h"

#include <algorithm>
#include <cmath>
#include <cstring>

#include <locale.h>

#include <wx/checkbox.h>
#include <wx/datetime.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/scrolwin.h>
#include <wx/sizer.h>
#include <wx/sstream.h>
#include <wx/stattext.h>
#include <wx/tokenzr.h>
#include <wx/txtstrm.h>
#include <wx/wfstream.h>
#include <wx/numformatter.h>
#include <wx/stdpaths.h>

#include "../EffectManager.h"
#include "FileNames.h"
#include "../../LabelTrack.h"
#include "Languages.h"
#include "../../NoteTrack.h"
#include "../../TimeTrack.h"
#include "../../prefs/SpectrogramSettings.h"
#include "PluginManager.h"
#include "Project.h"
#include "ProjectRate.h"
#include "../../ShuttleAutomation.h"
#include "../../ShuttleGetDefinition.h"
#include "../../ShuttleGui.h"
#include "TempDirectory.h"
#include "SyncLock.h"
#include "ViewInfo.h"
#include "../../WaveClip.h"
#include "../../WaveTrack.h"
#include "../../widgets/valnum.h"
#include "../../widgets/AudacityMessageBox.h"
#include "Prefs.h"
#include "../../prefs/GUIPrefs.h"
#include "../../tracks/playabletrack/wavetrack/ui/WaveTrackView.h"
#include "../../tracks/playabletrack/wavetrack/ui/WaveTrackViewConstants.h"
#include "../../widgets/ProgressDialog.h"

#include "../../widgets/FileDialog/FileDialog.h"

#ifndef nyx_returns_start_and_end_time
#error You need to update lib-src/libnyquist
#endif

#include <locale.h>
#include <ostream>
#include <sstream>
#include <float.h>

int NyquistEffect::mReentryCount = 0;

///////////////////////////////////////////////////////////////////////////////
//
// NyquistEffect
//
///////////////////////////////////////////////////////////////////////////////

NyquistEffect::NyquistEffect(const wxString &fName)
   : mParser{ std::make_unique<NyquistParser>(fName, *this) }
{
   auto &parser = GetParser();
   const auto &mFileName = parser.mFileName;
   auto &mInitError = parser.mInitError;
   const auto &mOK = parser.mOK;

   if (!(fName == NYQUIST_PROMPT_ID || fName == NYQUIST_WORKER_ID)) {
      mFileModified = mFileName.GetModificationTime();
      ParseFile();

      if (!mOK && mInitError.empty())
         mInitError = XO("Ill-formed Nyquist plug-in header");
   }
}

NyquistEffect::~NyquistEffect()
{
}

// ComponentInterface implementation

PluginPath NyquistEffect::GetPath() const
{
   return GetParser().mFileName.GetFullPath();
}

ComponentInterfaceSymbol NyquistEffect::GetSymbol() const
{
   return GetParser().mName;
}

VendorSymbol NyquistEffect::GetVendor() const
{
   return GetParser().mAuthor;
}

wxString NyquistEffect::GetVersion() const
{
   // Are Nyquist version strings really supposed to be translatable?
   // See commit a06e561 which used XO for at least one of them
   return GetParser().mReleaseVersion.Translation();
}

TranslatableString NyquistEffect::GetDescription() const
{
   return GetParser().mCopyright;
}

ManualPageID NyquistEffect::ManualPage() const
{
   return GetParser().mManPage;
}


std::pair<bool, FilePath> NyquistEffect::CheckHelpPage() const
{
   auto paths = NyquistEffect::GetNyquistSearchPath();
   wxString fileName;

   for (size_t i = 0, cnt = paths.size(); i < cnt; i++) {
      fileName = wxFileName(paths[i] + wxT("/") + GetParser().mHelpFile).GetFullPath();
      if (wxFileExists(fileName))
      {
         return { true, fileName };
      }
   }
   return { false, wxEmptyString };
}


FilePath NyquistEffect::HelpPage() const
{
   return mHelpPage;
}

// EffectDefinitionInterface implementation

EffectType NyquistEffect::GetType() const
{
   return GetParser().GetType();
}

EffectType NyquistEffect::GetClassification() const
{
   if (GetParser().mIsTool)
      return EffectTypeTool;
   return GetParser().mType;
}

EffectFamilySymbol NyquistEffect::GetFamily() const
{
   return NYQUISTEFFECTS_FAMILY;
}

bool NyquistEffect::IsInteractive() const
{
   return GetControls().size() != 0;
}

bool NyquistEffect::IsDefault() const
{
   return false;
}

bool NyquistEffect::VisitSettings(
   SettingsVisitor &visitor, EffectSettings &settings)
{
   if (auto pSa = dynamic_cast<ShuttleSetAutomation*>(&visitor))
      LoadSettings(*pSa->mpEap, settings);
   return true;
}

bool NyquistEffect::VisitSettings(
   ConstSettingsVisitor &visitor, const EffectSettings &settings) const
{
   // For now we assume Nyquist can do get and set better than VisitSettings can,
   // And so we ONLY use it for getting the signature.
   if (auto pGa = dynamic_cast<ShuttleGetAutomation*>(&visitor)) {
      SaveSettings(settings, *pGa->mpEap);
      return true;
   }
   else if (auto pSd = dynamic_cast<ShuttleGetDefinition*>(&visitor);
       !pSd)
      // must be the NullShuttle
      return true;

   // Get the "definition," only for the help or info commands
   if (mExternal)
      return true;

   return DoVisitSettings(visitor, settings);
}

bool NyquistEffect::DoVisitSettings(
   ConstSettingsVisitor &visitor, const EffectSettings &) const
{
   GetControls().Visit(GetBindings(), visitor);
   return true;
}

bool NyquistEffect::SaveSettings(
   const EffectSettings &settings, CommandParameters & parms) const
{
   return GetControls().Save(GetBindings(), parms);
}

bool NyquistEffect::LoadSettings(
   const CommandParameters & parms, EffectSettings &settings) const
{
   // To do: externalize state so const_cast isn't needed
   return const_cast<NyquistEffect*>(this)->DoLoadSettings(&parms, settings);
}

bool NyquistEffect::DoLoadSettings(
   const CommandParameters *pParms, EffectSettings &settings)
{
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
      badCount = GetControls().Load(GetBindings(), *pParms, kTestOnly);
      if (badCount > 0)
         return false;
   }

   badCount = GetControls().Load(GetBindings(), *pParms, kTestAndSet);
   // We never do anything with badCount here.
   // It might be non zero, for missing parameters, and we allow that,
   // and don't distinguish that from an out-of-range value.
   return true;
}

// Effect Implementation
bool NyquistEffect::Init()
{
   auto &parser = GetParser();
   const auto &mIsSpectral = parser.mIsSpectral;
   const auto &mFileName = parser.mFileName;
   auto &mMaxLen = parser.mMaxLen;

   // As of Audacity 2.1.2 rc1, 'spectral' effects are allowed only if
   // the selected track(s) are in a spectrogram view, and there is at
   // least one frequency bound and Spectral Selection is enabled for the
   // selected track(s) - (but don't apply to Nyquist Prompt).

   if (mIsSpectral) {
      auto *project = FindProject();
      bool bAllowSpectralEditing = false;
      bool hasSpectral = false;

      for ( auto t :
               TrackList::Get( *project ).Selected< const WaveTrack >() ) {
         // Find() not Get() to avoid creation-on-demand of views in case we are
         // only previewing
         auto pView = WaveTrackView::Find( t );
         if ( pView ) {
            const auto displays = pView->GetDisplays();
            if (displays.end() != std::find(
               displays.begin(), displays.end(),
               WaveTrackSubView::Type{ WaveTrackViewConstants::Spectrum, {} }))
               hasSpectral = true;
         }
         if ( hasSpectral &&
             (t->GetSpectrogramSettings().SpectralSelectionEnabled())) {
            bAllowSpectralEditing = true;
            break;
         }
      }

      if (!bAllowSpectralEditing || ((mF0 < 0.0) && (mF1 < 0.0))) {
         if (!hasSpectral) {
            Effect::MessageBox(
            XO("Enable track spectrogram view before\n"
            "applying 'Spectral' effects."),
            wxOK | wxICON_EXCLAMATION | wxCENTRE,
            XO("Error") );
         } else {
            Effect::MessageBox(
               XO("To use 'Spectral effects', enable 'Spectral Selection'\n"
                           "in the track Spectrogram settings and select the\n"
                           "frequency range for the effect to act on."),
               wxOK | wxICON_EXCLAMATION | wxCENTRE,
               XO("Error") );
         }
         return false;
      }
   }

   if (!mExternal) {
      //TODO: (bugs):
      // 1) If there is more than one plug-in with the same name, GetModificationTime may pick the wrong one.
      // 2) If the ;type is changed after the effect has been registered, the plug-in will appear in the wrong menu.

      //TODO: If we want to auto-add parameters from spectral selection,
      //we will need to modify this test.
      //Note that removing it stops the caching of parameter values,
      //(during this session).
      if (mFileName.GetModificationTime().IsLaterThan(mFileModified))
      {
         // If the effect has internal state, save and restore it.
         // If the effect is stateless, saving and restoring don't matter.
         auto dummySettings = MakeSettings();
         constexpr auto key = L"TemporarySettings";
         SaveUserPreset(key, dummySettings);

         mMaxLen = NYQ_MAX_LEN;
         ParseFile();
         mFileModified = mFileName.GetModificationTime();

         LoadUserPreset(key, dummySettings);
      }
   }

   return true;
}

bool NyquistEffect::Process(EffectInstance &, EffectSettings &settings)
{
   NyquistTrack nyquistTrack{ *this,
      (GetType() == EffectTypeProcess ? 0.5 : 1.0) / GetNumWaveGroups()
   };

   auto &parser = GetParser();
   const auto &mHelpFile = parser.mHelpFile;
   const auto &mVersion = parser.mVersion;
   const auto &mMaxLen = parser.mMaxLen;
   const auto &mTrace = parser.mTrace;
   const auto &mName = parser.mName;

   auto &environment = mEnvironment;
   const auto &mRedirectOutput = environment.mRedirectOutput;

   auto &mStop = environment.mStop;
   auto &mBreak = environment.mBreak;
   auto &mCont = environment.mCont;

   // Check for reentrant Nyquist commands.
   // I'm choosing to mark skipped Nyquist commands as successful even though
   // they are skipped.  The reason is that when Nyquist calls out to a chain,
   // and that chain contains Nyquist,  it will be clearer if the chain completes
   // skipping Nyquist, rather than doing nothing at all.
   if( mReentryCount > 0 )
      return true;

   // Restore the reentry counter (to zero) when we exit.
   auto countRestorer = valueRestorer( mReentryCount);
   mReentryCount++;

   bool success = true;
   int nEffectsSoFar = nEffectsDone;
   mProjectChanged = false;
   EffectManager & em = EffectManager::Get();
   em.SetSkipStateFlag(false);

   // This code was added in a fix for bug 2392 (no preview for Nyquist)
   // It was commented out in a fix for bug 2428 (no progress dialog from a macro)
   //if (mExternal) {
   //  mProgress->Hide();
   //}

   mOutputTime = 0;
   mCount = 0;

   mStop = false;
   mBreak = false;
   mCont = false;

   mTrackIndex = 0;

   // If in tool mode, then we don't do anything with the track and selection.
   const bool bOnePassTool = (GetType() == EffectTypeTool);

   // We must copy all the tracks, because Paste needs label tracks to ensure
   // correct sync-lock group behavior when the timeline is affected; then we just want
   // to operate on the selected wave tracks
   if ( !bOnePassTool )
      CopyInputTracks(true);

   mNumSelectedChannels = bOnePassTool
      ? 0
      : mOutputTracks->Selected< const WaveTrack >().size();

   TranslatableString initMessage;
   if (!mHelpFile.empty() && !mHelpFileExists)
      initMessage = XO(
"error: File \"%s\" specified in header but not found in plug-in path.\n")
         .Format( mHelpFile );
   auto scope{ environment.Scope(std::move(initMessage)) };

   using namespace NyquistFormatting;

   if (mVersion >= 4)
   {
      auto project = FindProject();

      mProps = wxEmptyString;

      mProps += wxString::Format(wxT("(putprop '*AUDACITY* (list %d %d %d) 'VERSION)\n"), AUDACITY_VERSION, AUDACITY_RELEASE, AUDACITY_REVISION);
      wxString lang = gPrefs->Read(wxT("/Locale/Language"), wxT(""));
      lang = (lang.empty())
         ? Languages::GetSystemLanguageCode(FileNames::AudacityPathList())
         : lang;
      mProps += wxString::Format(wxT("(putprop '*AUDACITY* \"%s\" 'LANGUAGE)\n"), lang);

      mProps += wxString::Format(wxT("(setf *DECIMAL-SEPARATOR* #\\%c)\n"), wxNumberFormatter::GetDecimalSeparator());

      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'BASE)\n"), EscapeString(FileNames::BaseDir()));
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'DATA)\n"), EscapeString(FileNames::DataDir()));
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'HELP)\n"), EscapeString(FileNames::HtmlHelpDir().RemoveLast()));
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'TEMP)\n"), EscapeString(TempDirectory::TempDir()));
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'SYS-TEMP)\n"), EscapeString(wxStandardPaths::Get().GetTempDir()));
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'DOCUMENTS)\n"), EscapeString(wxStandardPaths::Get().GetDocumentsDir()));
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'HOME)\n"), EscapeString(wxGetHomeDir()));

      auto paths = NyquistEffect::GetNyquistSearchPath();
      wxString list;
      for (size_t i = 0, cnt = paths.size(); i < cnt; i++)
      {
         list += wxT("\"") + EscapeString(paths[i]) + wxT("\" ");
      }
      list = list.RemoveLast();

      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* (list %s) 'PLUGIN)\n"), list);
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* (list %s) 'PLUG-IN)\n"), list);
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'USER-PLUG-IN)\n"),
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
      mProps += wxString::Format(wxT("(setf *SYSTEM-TIME* (list %d %d %d %d %d))\n"),
                                 year, doy, now.GetHour(), now.GetMinute(), now.GetSecond());

      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* \"%s\" 'DATE)\n"), now.FormatDate());
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* \"%s\" 'TIME)\n"), now.FormatTime());
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* \"%s\" 'ISO-DATE)\n"), now.FormatISODate());
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* \"%s\" 'ISO-TIME)\n"), now.FormatISOTime());
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* %d 'YEAR)\n"), year);
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* %d 'DAY)\n"), dom);   // day of month
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* %d 'MONTH)\n"), month);
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* \"%s\" 'MONTH-NAME)\n"), now.GetMonthName(month));
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* \"%s\" 'DAY-NAME)\n"), now.GetWeekDayName(day));

      mProps += wxString::Format(wxT("(putprop '*PROJECT* %d 'PROJECTS)\n"),
         (int) AllProjects{}.size());
      mProps += wxString::Format(wxT("(putprop '*PROJECT* \"%s\" 'NAME)\n"), EscapeString(project->GetProjectName()));

      int numTracks = 0;
      int numWave = 0;
      int numLabel = 0;
      int numMidi = 0;
      int numTime = 0;
      wxString waveTrackList;   // track positions of selected audio tracks.

      {
         auto countRange = TrackList::Get( *project ).Leaders();
         for (auto t : countRange) {
            t->TypeSwitch( [&](const WaveTrack *) {
               numWave++;
               if (t->GetSelected())
                  waveTrackList += wxString::Format(wxT("%d "), 1 + numTracks);
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
      mProps += wxString::Format(wxT("(putprop '*PROJECT* (float %s) 'RATE)\n"),
         Internat::ToString(ProjectRate::Get(*project).GetRate()));
      mProps += wxString::Format(wxT("(putprop '*PROJECT* %d 'TRACKS)\n"), numTracks);
      mProps += wxString::Format(wxT("(putprop '*PROJECT* %d 'WAVETRACKS)\n"), numWave);
      mProps += wxString::Format(wxT("(putprop '*PROJECT* %d 'LABELTRACKS)\n"), numLabel);
      mProps += wxString::Format(wxT("(putprop '*PROJECT* %d 'MIDITRACKS)\n"), numMidi);
      mProps += wxString::Format(wxT("(putprop '*PROJECT* %d 'TIMETRACKS)\n"), numTime);

      double previewLen = 6.0;
      gPrefs->Read(wxT("/AudioIO/EffectsPreviewLen"), &previewLen);
      mProps += wxString::Format(wxT("(putprop '*PROJECT* (float %s) 'PREVIEW-DURATION)\n"),
                                 Internat::ToString(previewLen));

      // *PREVIEWP* is true when previewing (better than relying on track view).
      wxString isPreviewing = (this->IsPreviewing())? wxT("T") : wxT("NIL");
      mProps += wxString::Format(wxT("(setf *PREVIEWP* %s)\n"), isPreviewing);

      mProps += wxString::Format(wxT("(putprop '*SELECTION* (float %s) 'START)\n"),
                                 Internat::ToString(mT0));
      mProps += wxString::Format(wxT("(putprop '*SELECTION* (float %s) 'END)\n"),
                                 Internat::ToString(mT1));
      mProps += wxString::Format(wxT("(putprop '*SELECTION* (list %s) 'TRACKS)\n"), waveTrackList);
      mProps += wxString::Format(wxT("(putprop '*SELECTION* %d 'CHANNELS)\n"), mNumSelectedChannels);
   }

   // Nyquist Prompt does not require a selection, but effects do.
   if (!bOnePassTool && (mNumSelectedChannels == 0)) {
      auto message = XO("Audio selection required.");
      Effect::MessageBox(
         message,
         wxOK | wxCENTRE | wxICON_EXCLAMATION,
         XO("Nyquist Error") );
   }

   std::optional<TrackIterRange<WaveTrack>> pRange;
   if (!bOnePassTool)
      pRange.emplace(mOutputTracks->Selected< WaveTrack >() + &Track::IsLeader);

   // Keep track of whether the current track is first selected in its sync-lock group
   // (we have no idea what the length of the returned audio will be, so we have
   // to handle sync-lock group behavior the "old" way).
   mFirstInGroup = true;
   Track *gtLast = NULL;

   for (;
        bOnePassTool || pRange->first != pRange->second;
        (void) (!pRange || (++pRange->first, true))
   ) {
      if (!nyquistTrack.NextTrack(pRange ? *pRange->first : nullptr,
         mT0, mT1, mMaxLen)
      ){
         success = false;
         goto finish;
      }

      auto subscope{ environment.Subscope(scope) };

      if ( (mT1 >= mT0) || bOnePassTool ) {
         if (bOnePassTool) {
         }
         else {
            // Check whether we're in the same group as the last selected track
            Track *gt = *SyncLock::Group(nyquistTrack.CurTracks()[0]).first;
            mFirstInGroup = !gtLast || (gtLast != gt);
            gtLast = gt;
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

         if (mVersion >= 4)
         {
            mPerTrackProps = wxEmptyString;
            wxString lowHz = wxT("nil");
            wxString highHz = wxT("nil");
            wxString centerHz = wxT("nil");
            wxString bandwidth = wxT("nil");

#if defined(EXPERIMENTAL_SPECTRAL_EDITING)
            if (mF0 >= 0.0) {
               lowHz.Printf(wxT("(float %s)"), Internat::ToString(mF0));
            }

            if (mF1 >= 0.0) {
               highHz.Printf(wxT("(float %s)"), Internat::ToString(mF1));
            }

            if ((mF0 >= 0.0) && (mF1 >= 0.0)) {
               centerHz.Printf(wxT("(float %s)"), Internat::ToString(sqrt(mF0 * mF1)));
            }

            if ((mF0 > 0.0) && (mF1 >= mF0)) {
               // with very small values, bandwidth calculation may be inf.
               // (Observed on Linux)
               double bw = log(mF1 / mF0) / log(2.0);
               if (!std::isinf(bw)) {
                  bandwidth.Printf(wxT("(float %s)"), Internat::ToString(bw));
               }
            }

#endif
            mPerTrackProps += wxString::Format(wxT("(putprop '*SELECTION* %s 'LOW-HZ)\n"), lowHz);
            mPerTrackProps += wxString::Format(wxT("(putprop '*SELECTION* %s 'CENTER-HZ)\n"), centerHz);
            mPerTrackProps += wxString::Format(wxT("(putprop '*SELECTION* %s 'HIGH-HZ)\n"), highHz);
            mPerTrackProps += wxString::Format(wxT("(putprop '*SELECTION* %s 'BANDWIDTH)\n"), bandwidth);
         }

         success = ProcessOne(environment, nyquistTrack);

         // Reset previous locale
         wxSetlocale(LC_NUMERIC, prevlocale);

         if (!success || bOnePassTool) {
            goto finish;
         }
         nyquistTrack.AccumulateProgress();
      }

      mCount += nyquistTrack.CurNumChannels();
   }

   if (mOutputTime > 0.0) {
      mT1 = mT0 + mOutputTime;
   }

finish:

   // Show debug window if trace set in plug-in header and something to show.
   auto &debugOutput = environment.DebugOutput();
   mDebug = (mDebug || (mTrace && !debugOutput.empty()));

   if (mDebug && !mRedirectOutput) {
      NyquistOutputDialog dlog(mUIParent, -1,
                               mName,
                               XO("Debug Output: "),
                               Verbatim(debugOutput));
      dlog.CentreOnParent();
      dlog.ShowModal();
   }

   // Has rug been pulled from under us by some effect done within Nyquist??
   if( !bOnePassTool && ( nEffectsSoFar == nEffectsDone ))
      ReplaceProcessedTracks(success);
   else{
      ReplaceProcessedTracks(false); // Do not use the results.
      // Selection is to be set to whatever it is in the project.
      auto project = FindProject();
      if (project) {
         auto &selectedRegion = ViewInfo::Get( *project ).selectedRegion;
         mT0 = selectedRegion.t0();
         mT1 = selectedRegion.t1();
      }
      else {
         mT0 = 0;
         mT1 = -1;
      }

   }

   if (!mProjectChanged)
      em.SetSkipStateFlag(true);

   return success;
}

int NyquistEffect::ShowHostInterface(
   wxWindow &parent, const EffectDialogFactory &factory,
   std::shared_ptr<EffectInstance> &pInstance, EffectSettingsAccess &access,
   bool forceModal)
{
   int res = Effect::ShowHostInterface(
      parent, factory, pInstance, access, forceModal);
   // Remember if the user clicked debug
   mDebug = (res == eDebugID);
   return res;
}

bool NyquistEffect::EnablesDebug() const
{
   return GetParser().mDebugButton;
}

// NyquistEffect implementation

bool NyquistEffect::ProcessOne(
   NyquistEnvironment &environment, NyquistTrack &nyquistTrack)
{
   auto &parser = GetParser();
   const auto &mVersion = parser.mVersion;
   const auto &mTrace = parser.mTrace;
   const auto &mIsSal = parser.mIsSal;
   const auto &mCompiler = parser.mCompiler;
   const auto &mName = parser.mName;
   const auto &mMergeClips = parser.mMergeClips;
   const auto &mRestoreSplits = parser.mRestoreSplits;

   const auto mCurTrack = nyquistTrack.CurTracks();
   const auto mCurLen = nyquistTrack.CurLength();
   const auto mCurNumChannels = nyquistTrack.CurNumChannels();

   nyx_rval rval;

   wxString cmd;
   cmd += wxT("(snd-set-latency  0.1)");

   // A tool may be using AUD-DO which will potentially invalidate *TRACK*
   // so tools do not get *TRACK*.
   if (GetType() == EffectTypeTool)
      cmd += wxT("(setf S 0.25)\n");  // No Track.
   else if (mVersion >= 4) {
      nyx_set_audio_name("*TRACK*");
      cmd += wxT("(setf S 0.25)\n");
   }
   else {
      nyx_set_audio_name("S");
      cmd += wxT("(setf *TRACK* '*unbound*)\n");
   }

   if (mVersion >= 4) {
      cmd += mProps;
      cmd += mPerTrackProps;
   }

   using namespace NyquistFormatting;

   if( (mVersion >= 4) && (GetType() != EffectTypeTool) ) {
      // Set the track TYPE and VIEW properties
      wxString type;
      wxString view;
      wxString bitFormat;
      wxString spectralEditp;

      mCurTrack[0]->TypeSwitch(
         [&](const WaveTrack *wt) {
            type = wxT("wave");
            spectralEditp = mCurTrack[0]->GetSpectrogramSettings().SpectralSelectionEnabled()? wxT("T") : wxT("NIL");
            view = wxT("NIL");
            // Find() not Get() to avoid creation-on-demand of views in case we are
            // only previewing
            if ( const auto pView = WaveTrackView::Find( wt ) ) {
               auto displays = pView->GetDisplays();
               auto format = [&]( decltype(displays[0]) display ) {
                  // Get the English name of the view type, without menu codes,
                  // as a string that Lisp can examine
                  return wxString::Format( wxT("\"%s\""),
                     display.name.Stripped().Debug() );
               };
               if (displays.empty())
                  ;
               else if (displays.size() == 1)
                  view = format( displays[0] );
               else {
                  view = wxT("(list");
                  for ( auto display : displays )
                     view += wxString(wxT(" ")) + format( display );
                  view += wxT(")");
               }
            }
         },
#if defined(USE_MIDI)
         [&](const NoteTrack *) {
            type = wxT("midi");
            view = wxT("\"Midi\"");
         },
#endif
         [&](const LabelTrack *) {
            type = wxT("label");
            view = wxT("\"Label\"");
         },
         [&](const TimeTrack *) {
            type = wxT("time");
            view = wxT("\"Time\"");
         }
      );

      cmd += wxString::Format(wxT("(putprop '*TRACK* %d 'INDEX)\n"), ++mTrackIndex);
      cmd += wxString::Format(wxT("(putprop '*TRACK* \"%s\" 'NAME)\n"), EscapeString(mCurTrack[0]->GetName()));
      cmd += wxString::Format(wxT("(putprop '*TRACK* \"%s\" 'TYPE)\n"), type);
      // Note: "View" property may change when Audacity's choice of track views has stabilized.
      cmd += wxString::Format(wxT("(putprop '*TRACK* %s 'VIEW)\n"), view);
      cmd += wxString::Format(wxT("(putprop '*TRACK* %d 'CHANNELS)\n"), mCurNumChannels);

      //NOTE: Audacity 2.1.3 True if spectral selection is enabled regardless of track view.
      cmd += wxString::Format(wxT("(putprop '*TRACK* %s 'SPECTRAL-EDIT-ENABLED)\n"), spectralEditp);

      auto channels = TrackList::Channels( mCurTrack[0] );
      double startTime = channels.min( &Track::GetStartTime );
      double endTime = channels.max( &Track::GetEndTime );

      cmd += wxString::Format(wxT("(putprop '*TRACK* (float %s) 'START-TIME)\n"),
                              Internat::ToString(startTime));
      cmd += wxString::Format(wxT("(putprop '*TRACK* (float %s) 'END-TIME)\n"),
                              Internat::ToString(endTime));
      cmd += wxString::Format(wxT("(putprop '*TRACK* (float %s) 'GAIN)\n"),
                              Internat::ToString(mCurTrack[0]->GetGain()));
      cmd += wxString::Format(wxT("(putprop '*TRACK* (float %s) 'PAN)\n"),
                              Internat::ToString(mCurTrack[0]->GetPan()));
      cmd += wxString::Format(wxT("(putprop '*TRACK* (float %s) 'RATE)\n"),
                              Internat::ToString(mCurTrack[0]->GetRate()));

      switch (mCurTrack[0]->GetSampleFormat())
      {
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
      cmd += wxString::Format(wxT("(putprop '*TRACK* %s 'FORMAT)\n"), bitFormat);

      float maxPeakLevel = 0.0;  // Deprecated as of 2.1.3
      wxString clips, peakString, rmsString;
      for (size_t i = 0; i < mCurNumChannels; i++) {
         auto ca = mCurTrack[i]->SortedClipArray();
         float maxPeak = 0.0;

         // A list of clips for mono, or an array of lists for multi-channel.
         if (mCurNumChannels > 1) {
            clips += wxT("(list ");
         }
         // Each clip is a list (start-time, end-time)
         // Limit number of clips added to avoid argument stack overflow error (bug 2300).
         for (size_t i=0; i<ca.size(); i++) {
            if (i < 1000) {
               clips += wxString::Format(wxT("(list (float %s) (float %s))"),
                                         Internat::ToString(ca[i]->GetPlayStartTime()),
                                         Internat::ToString(ca[i]->GetPlayEndTime()));
            } else if (i == 1000) {
               // If final clip is NIL, plug-in developer knows there are more than 1000 clips in channel.
               clips += "NIL";
            } else if (i > 1000) {
               break;
            }
         }
         if (mCurNumChannels > 1) clips += wxT(" )");

         float min, max;
         auto pair = mCurTrack[i]->GetMinMax(mT0, mT1); // may throw
         min = pair.first, max = pair.second;
         maxPeak = wxMax(wxMax(fabs(min), fabs(max)), maxPeak);
         maxPeakLevel = wxMax(maxPeakLevel, maxPeak);

         // On Debian, NaN samples give maxPeak = 3.40282e+38 (FLT_MAX)
         if (!std::isinf(maxPeak) && !std::isnan(maxPeak) && (maxPeak < FLT_MAX)) {
            peakString += wxString::Format(wxT("(float %s) "), Internat::ToString(maxPeak));
         } else {
            peakString += wxT("nil ");
         }

         float rms = mCurTrack[i]->GetRMS(mT0, mT1); // may throw
         if (!std::isinf(rms) && !std::isnan(rms)) {
            rmsString += wxString::Format(wxT("(float %s) "), Internat::ToString(rms));
         } else {
            rmsString += wxT("NIL ");
         }
      }
      // A list of clips for mono, or an array of lists for multi-channel.
      cmd += wxString::Format(wxT("(putprop '*TRACK* %s%s ) 'CLIPS)\n"),
                              (mCurNumChannels == 1) ? wxT("(list ") : wxT("(vector "),
                              clips);

      (mCurNumChannels > 1)?
         cmd += wxString::Format(wxT("(putprop '*SELECTION* (vector %s) 'PEAK)\n"), peakString) :
         cmd += wxString::Format(wxT("(putprop '*SELECTION* %s 'PEAK)\n"), peakString);

      if (!std::isinf(maxPeakLevel) && !std::isnan(maxPeakLevel) && (maxPeakLevel < FLT_MAX)) {
         cmd += wxString::Format(wxT("(putprop '*SELECTION* (float %s) 'PEAK-LEVEL)\n"),
                                 Internat::ToString(maxPeakLevel));
      }

      (mCurNumChannels > 1)?
         cmd += wxString::Format(wxT("(putprop '*SELECTION* (vector %s) 'RMS)\n"), rmsString) :
         cmd += wxString::Format(wxT("(putprop '*SELECTION* %s 'RMS)\n"), rmsString);
   }

   // If in tool mode, then we don't do anything with the track and selection.
   if (GetType() == EffectTypeTool) {
      nyx_set_audio_params(44100, 0);
   }
   else if (GetType() == EffectTypeGenerate) {
      nyx_set_audio_params(mCurTrack[0]->GetRate(), 0);
   }
   else {
      auto curLen = mCurLen.as_long_long();
      nyx_set_audio_params(mCurTrack[0]->GetRate(), curLen);

      nyx_set_input_audio(NyquistTrack::StaticGetCallback, &nyquistTrack,
         (int)mCurNumChannels, curLen, mCurTrack[0]->GetRate());
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
   }
   else {
      // Explicitly disable backtrace and prevent values
      // from being carried through to the output.
      // This should be the final command before evaluating the Nyquist script.
      cmd += wxT("(setf *tracenable* NIL)\n");
   }

   cmd += GetControls().Expression(GetBindings());

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
   }
   else {
      cmd += mCmd;
   }

   // Evaluate the expression, which may invoke the get callback, but often does
   // not, leaving that to delayed evaluation of the output sound
   rval = nyx_eval_expression(cmd.mb_str(wxConvUTF8));

   // If we're not showing debug window, log errors and warnings:
   const auto &output = environment.DebugOutput();
   if (!output.empty() && !mDebug && !mTrace) {
      /* i18n-hint: An effect "returned" a message.*/
      wxLogMessage(wxT("\'%s\' returned:\n%s"),
         mName.Translation(), output);
   }

   // Audacity has no idea how long Nyquist processing will take, but
   // can monitor audio being returned.
   // Anything other than audio should be returned almost instantly
   // so notify the user that process has completed (bug 558)
   if ((rval != nyx_audio) && ((mCount + mCurNumChannels) == mNumSelectedChannels)) {
      if (mCurNumChannels == 1) {
         TrackProgress(mCount, 1.0, XO("Processing complete."));
      }
      else {
         TrackGroupProgress(mCount, 1.0, XO("Processing complete."));
      }
   }

   if ((rval == nyx_audio) && (GetType() == EffectTypeTool)) {
      // Catch this first so that we can also handle other errors.
      environment.PrependDebug(
         /* i18n-hint: Don't translate ';type tool'.  */
         XO("';type tool' effects cannot return audio from Nyquist.\n"));
      rval = nyx_error;
   }

   if ((rval == nyx_labels) && (GetType() == EffectTypeTool)) {
      // Catch this first so that we can also handle other errors.
      environment.PrependDebug(
         /* i18n-hint: Don't translate ';type tool'.  */
         XO("';type tool' effects cannot return labels from Nyquist.\n"));
      rval = nyx_error;
   }

   if (rval == nyx_error) {
      // Return value is not valid type.
      // Show error in debug window if trace enabled, otherwise log.
      if (mTrace) {
         environment.PrependDebug(
            /* i18n-hint: "%s" is replaced by name of plug-in.*/
            XO("nyx_error returned from %s.\n")
               .Format( mName.empty() ? XO("plug-in") : mName ));
         mDebug = true;
      }
      else {
         wxLogMessage(
            "Nyquist returned nyx_error:\n%s", environment.DebugOutput());
      }
      return false;
   }

   if (rval == nyx_list) {
      wxLogMessage("Nyquist returned nyx_list");
      if (GetType() == EffectTypeTool) {
         mProjectChanged = true;
      } else {
         Effect::MessageBox(XO("Nyquist returned a list.") );
      }
      return true;
   }

   const bool acceptAll = AcceptsAllNyquistTypes();

   if (rval == nyx_string) {
      // Assume the string has already been translated within the Lisp runtime
      // if necessary, by one of the gettext functions defined below, before it
      // is communicated back to C++
      auto msg = Verbatim( NyquistToWxString(nyx_get_string()) );
      if (!msg.empty()) { // Empty string may be used as a No-Op return value.
         Effect::MessageBox( msg );
      }
      else if (GetType() == EffectTypeTool) {
         // ;tools may change the project with aud-do commands so
         // it is essential that the state is added to history.
         mProjectChanged = true;
         return true;
      }
      else {
         // A true no-op.
         return true;
      }

      return acceptAll;
   }

   if (rval == nyx_double) {
      auto str = XO("Nyquist returned the value: %f")
         .Format(nyx_get_double());
      Effect::MessageBox( str );
      return acceptAll;
   }

   if (rval == nyx_int) {
      auto str = XO("Nyquist returned the value: %d")
         .Format(nyx_get_int());
      Effect::MessageBox( str );
      return acceptAll;
   }

   if (rval == nyx_labels) {
      mProjectChanged = true;
      unsigned int numLabels = nyx_get_num_labels();
      unsigned int l;
      auto ltrack = * mOutputTracks->Any< LabelTrack >().begin();
      if (!ltrack) {
         auto newTrack = std::make_shared<LabelTrack>();
         //new track name should be unique among the names in the list of input tracks, not output
         newTrack->SetName(inputTracks()->MakeUniqueTrackName(LabelTrack::GetDefaultName()));
         ltrack = static_cast<LabelTrack*>(
            AddToOutputTracks(newTrack));
      }

      for (l = 0; l < numLabels; l++) {
         double t0, t1;
         const char *str;

         // PRL:  to do:
         // let Nyquist analyzers define more complicated selections
         nyx_get_label(l, &t0, &t1, &str);

         ltrack->AddLabel(SelectedRegion(t0 + mT0, t1 + mT0), UTF8CTOWX(str));
      }
      return acceptAll;
   }

   wxASSERT(rval == nyx_audio);

   int outChannels = nyx_get_audio_num_channels();
   if (outChannels > (int)mCurNumChannels) {
      Effect::MessageBox( XO("Nyquist returned too many audio channels.\n") );
      return false;
   }

   if (outChannels == -1) {
      Effect::MessageBox(
         XO("Nyquist returned one audio channel as an array.\n") );
      return false;
   }

   if (outChannels == 0) {
      Effect::MessageBox( XO("Nyquist returned an empty array.\n") );
      return false;
   }

   // May throw other errors
   auto outputTracks = nyquistTrack.GetResult(outChannels, mOutputTime);
   if (outputTracks.empty())
      return false;

   for (size_t i = 0; i < mCurNumChannels; i++) {
      WaveTrack *out;

      if (outChannels == (int)mCurNumChannels)
         out = outputTracks[i].get();
      else
         out = outputTracks[0].get();

      if (mMergeClips < 0) {
         // Use sample counts to determine default behaviour - times will rarely be equal.
         bool bMergeClips = (out->TimeToLongSamples(mT0) + out->TimeToLongSamples(mOutputTime) ==
                                                                     out->TimeToLongSamples(mT1));
         mCurTrack[i]->ClearAndPaste(mT0, mT1, out, mRestoreSplits, bMergeClips);
      }
      else {
         mCurTrack[i]->ClearAndPaste(mT0, mT1, out, mRestoreSplits, mMergeClips != 0);
      }

      // If we were first in the group adjust non-selected group tracks
      if (mFirstInGroup) {
         for (auto t : SyncLock::Group(mCurTrack[i]))
         {
            if (!t->GetSelected() && SyncLock::IsSyncLockSelected(t)) {
               t->SyncLockAdjust(mT1, mT0 + out->GetEndTime());
            }
         }
      }

      // Only the first channel can be first in its group
      mFirstInGroup = false;
   }

   mProjectChanged = true;
   return true;
}

bool NyquistEffect::AcceptsAllNyquistTypes()
{
   // True if not process type.
   // If not returning audio from process effect,
   // return first result then stop (disables preview)
   return GetType() != EffectTypeProcess;
}

// ============================================================================
// NyquistEffect Implementation
// ============================================================================

wxString NyquistEffect::NyquistToWxString(const char *nyqString)
{
    wxString str(nyqString, wxConvUTF8);
    if (nyqString != NULL && nyqString[0] && str.empty()) {
        // invalid UTF-8 string, convert as Latin-1
        str = _("[Warning: Nyquist returned invalid UTF-8 string, converted here as Latin-1]");
       // TODO: internationalization of strings from Nyquist effects, at least
       // from those shipped with Audacity
        str += LAT1CTOWX(nyqString);
    }
    return str;
}

void NyquistEffect::RedirectOutput()
{
   mEnvironment.mRedirectOutput = true;
}

void NyquistEffect::SetCommand(const wxString &cmd)
{
   mExternal = true;

   if (cmd.size()) {
      ParseCommand(cmd);
   }
}

void NyquistEffect::Break()
{
   mEnvironment.mBreak = true;
}

void NyquistEffect::Continue()
{
   mEnvironment.mCont = true;
}

void NyquistEffect::Stop()
{
   mEnvironment.mStop = true;
}

NyquistUIControls &NyquistEffect::GetControls()
{
   return GetParser().mControls;
}

const NyquistUIControls &NyquistEffect::GetControls() const
{
   return GetParser().mControls;
}

NyquistBindings &NyquistEffect::GetBindings()
{
   return GetParser().mBindings;
}

const NyquistBindings &NyquistEffect::GetBindings() const
{
   return GetParser().mBindings;
}

void NyquistEffect::SetControls(std::vector</*const*/ NyqControl> controls)
{
   GetControls().SetControls(move(controls));
}

void NyquistEffect::SetBindings(std::vector<NyqValue> bindings)
{
   GetBindings() = move(bindings);
}

std::vector</*const*/ NyqControl> NyquistEffect::MoveControls()
{
   return GetControls().MoveControls();
}

std::vector<NyqValue> NyquistEffect::MoveBindings()
{
   return move(GetBindings());
}

const TranslatableString &NyquistEffect::InitializationError() const
{
   return GetParser().mInitError;
}

bool NyquistEffect::ParseProgram(wxInputStream & stream)
{
   auto &parser = GetParser();
   auto &mInitError = parser.mInitError;
   auto &mIsSal = parser.mIsSal;
   auto &mCategories = parser.mCategories;
   auto &mIsSpectral = parser.mIsSpectral;
   auto &mTrace = parser.mTrace;
   auto &mManPage = parser.mManPage;
   auto &mHelpFile = parser.mHelpFile;
   auto &mDebugButton = parser.mDebugButton;
   auto &mType = parser.mType;
   auto &mFoundType = parser.mFoundType;
   auto &mLinear = parser.mLinear;
   auto &mPreview = parser.mPreview;

   if (!stream.IsOk())
   {
      mInitError = XO("Could not open file");
      return false;
   }

   wxTextInputStream pgm(stream, wxT(" \t"), wxConvAuto());

   mCmd = wxT("");
   mCmd.Alloc(10000);
   mIsSal = false;
   GetControls().clear();
   GetBindings().clear();
   mCategories.clear();
   mIsSpectral = false;
   mManPage = wxEmptyString; // If not wxEmptyString, must be a page in the Audacity manual.
   mHelpFile = wxEmptyString; // If not wxEmptyString, must be a valid HTML help file.
   mHelpFileExists = false;
   mDebug = false;
   mTrace = false;
   mDebugButton = true;    // Debug button enabled by default.
   GetControls().mEnablePreview = true;  // Preview button enabled by default.

   // Bug 1934.
   // All Nyquist plug-ins should have a ';type' field, but if they don't we default to
   // being an Effect.
   mType = EffectTypeProcess;

   mFoundType = false;
   while (!stream.Eof() && stream.IsOk())
   {
      wxString line = pgm.ReadLine();
      if (line.length() > 1 &&
          // New in 2.3.0:  allow magic comment lines to start with $
          // The trick is that xgettext will not consider such lines comments
          // and will extract the strings they contain
          (line[0] == wxT(';') || line[0] == wxT('$')) )
      {
         NyquistParser::Tokenizer tzer;
         unsigned nLines = 1;
         bool done;
         // Allow continuations within control lines.
         bool control =
            line[0] == wxT('$') || line.StartsWith( wxT(";control") );
         do
            done = GetParser().Parse(tzer, line, !control || stream.Eof(), nLines == 1);
         while(!done &&
            (line = pgm.ReadLine(), ++nLines, true));

         // Don't pass these lines to the interpreter, so it doesn't get confused
         // by $, but pass blanks,
         // so that SAL effects compile with proper line numbers
         while (nLines --)
            mCmd += wxT('\n');
      }
      else
      {
         if(!mFoundType && line.length() > 0) {
            if (line[0] == wxT('(') ||
                (line[0] == wxT('#') && line.length() > 1 && line[1] == wxT('|')))
            {
               mIsSal = false;
               mFoundType = true;
            }
            else if (line.Upper().Find(wxT("RETURN")) != wxNOT_FOUND)
            {
               mIsSal = true;
               mFoundType = true;
            }
         }
         mCmd += line + wxT("\n");
      }
   }
   if (!mFoundType && !RecoverParseTypeFailed())
      return false;

   const auto helpStuff = CheckHelpPage();
   mHelpFileExists = helpStuff.first;
   mHelpPage       = helpStuff.second;

   SetLinearEffectFlag(mLinear);
   SetPreviewFullSelectionFlag(mPreview);

   return true;
}

bool NyquistEffect::RecoverParseTypeFailed()
{
   // Just throw it at Nyquist to see what happens
   return true;
}

void NyquistEffect::ParseFile()
{
   wxFileInputStream rawStream(GetParser().mFileName.GetFullPath());
   wxBufferedInputStream stream(rawStream, 10000);

   ParseProgram(stream);
}

bool NyquistEffect::ParseCommand(const wxString & cmd)
{
   wxStringInputStream stream(cmd + wxT(" "));

   return ParseProgram(stream);
}

FilePaths NyquistEffect::GetNyquistSearchPath()
{
   const auto &audacityPathList = FileNames::AudacityPathList();
   FilePaths pathList;

   for (size_t i = 0; i < audacityPathList.size(); i++)
   {
      wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
      FileNames::AddUniquePathToPathList(prefix + wxT("nyquist"), pathList);
      FileNames::AddUniquePathToPathList(prefix + wxT("plugins"), pathList);
      FileNames::AddUniquePathToPathList(prefix + wxT("plug-ins"), pathList);
   }
   pathList.push_back(FileNames::PlugInDir());

   return pathList;
}

bool NyquistEffect::TransferDataToWindow(const EffectSettings &)
{
   mUIParent->TransferDataToWindow();
   return GetControls().UpdateUI();
}

bool NyquistEffect::TransferDataFromWindow(EffectSettings &)
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
      return false;
   return GetControls().ValidateUI();
}

std::unique_ptr<EffectUIValidator> NyquistEffect::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &)
{
   wxScrolledWindow *scroller = S.Style(wxVSCROLL | wxTAB_TRAVERSAL)
      .StartScroller(2);
   {
      S.StartMultiColumn(4);
      {
         GetControls().Populate(S, GetSelectionFormat(), mProjectRate);
      }
      S.EndMultiColumn();
   }
   S.EndScroller();

   scroller->SetScrollRate(0, 20);

   // This fools NVDA into not saying "Panel" when the dialog gets focus
   scroller->SetName(wxT("\a"));
   scroller->SetLabel(wxT("\a"));
   return nullptr;
}

// NyquistEffect implementation

bool NyquistEffect::IsOk()
{
   return GetParser().mOK;
}

///////////////////////////////////////////////////////////////////////////////
//
// NyquistOutputDialog
//
///////////////////////////////////////////////////////////////////////////////


BEGIN_EVENT_TABLE(NyquistOutputDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, NyquistOutputDialog::OnOk)
END_EVENT_TABLE()

NyquistOutputDialog::NyquistOutputDialog(wxWindow * parent, wxWindowID id,
                                       const TranslatableString & title,
                                       const TranslatableString & prompt,
                                       const TranslatableString &message)
: wxDialogWrapper{ parent, id, title, wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER }
{
   SetName();

   ShuttleGui S{ this, eIsCreating };
   {
      S.SetBorder(10);

      S.AddVariableText( prompt, false, wxALIGN_LEFT | wxLEFT | wxTOP | wxRIGHT );

      // TODO: use ShowInfoDialog() instead.
      // Beware this dialog MUST work with screen readers.
      S.Prop( 1 )
         .Position(wxEXPAND | wxALL)
         .MinSize( { 480, 250 } )
         .Style(wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH)
         .AddTextWindow( message.Translation() );

      S.SetBorder( 5 );

      S.StartHorizontalLay(wxALIGN_CENTRE | wxLEFT | wxBOTTOM | wxRIGHT, 0 );
      {
         /* i18n-hint: In most languages OK is to be translated as OK.  It appears on a button.*/
         S.Id(wxID_OK).AddButton( XXO("OK"), wxALIGN_CENTRE, true );
      }
      S.EndHorizontalLay();

   }

   SetAutoLayout(true);
   GetSizer()->Fit(this);
   GetSizer()->SetSizeHints(this);
}

// ============================================================================
// NyquistOutputDialog implementation
// ============================================================================

void NyquistOutputDialog::OnOk(wxCommandEvent & /* event */)
{
   EndModal(wxID_OK);
}
