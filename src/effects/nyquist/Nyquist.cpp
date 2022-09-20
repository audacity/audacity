/**********************************************************************

  Audacity: A Digital Audio Editor

  Nyquist.cpp

  Dominic Mazzoni

******************************************************************//**

\class NyquistEffect
\brief An Effect that calls up a Nyquist (XLISP) plug-in, i.e. many possible
effects from this one class.

*//*******************************************************************/
#include "Nyquist.h"
#include "NyquistProgram.h"
#include "nyx.h"

#include <algorithm>
#include <cmath>
#include <cstring>

#include <locale.h>

#include <wx/checkbox.h>
#include <wx/datetime.h>
#include <wx/intl.h>
#include <wx/scrolwin.h>
#include <wx/sstream.h>
#include <wx/stattext.h>
#include <wx/tokenzr.h>
#include <wx/wfstream.h>

#include "FileNames.h"
#include "../../prefs/SpectrogramSettings.h"
#include "PluginManager.h"
#include "../../ShuttleAutomation.h"
#include "../../ShuttleGetDefinition.h"
#include "../../ShuttleGui.h"
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
   : mProgram{ std::make_unique<NyquistProgram>(fName, *this) }
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

FilePath NyquistEffect::HelpPage() const
{
   return mProgram->mHelpPage;
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

   // If in tool mode, then we don't do anything with the track and selection.
   const bool bOnePassTool = (GetType() == EffectTypeTool);

   // We must copy all the tracks, because Paste needs label tracks to ensure
   // correct sync-lock group behavior when the timeline is affected; then we just want
   // to operate on the selected wave tracks
   if ( !bOnePassTool )
      CopyInputTracks(true);

   NyquistProgram::EffectContext eContext{
      inputTracks(), mOutputTracks.get(),
      mUIParent,
      GetNumWaveGroups(),
      IsPreviewing(),
      mF0, mF1,
      mT0, mT1,
      mDebug
   };
   NyquistProgram::Context context{ eContext,
      // mNumSelectedChannels
      bOnePassTool
         ? 0
         : (unsigned)mOutputTracks->Selected< const WaveTrack >().size(),
      AcceptsAllNyquistTypes(),
      mExternal
   };

   return mProgram->Process(FindProject(), mEnvironment, context, settings);
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
   mDebug = false;
   auto result = mProgram->Parse(stream);
   SetLinearEffectFlag(mProgram->mLinear);
   SetPreviewFullSelectionFlag(mProgram->mPreview);
   return result;
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
