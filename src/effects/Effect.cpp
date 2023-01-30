/**********************************************************************

  Audacity: A Digital Audio Editor

  Effect.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

*******************************************************************//**

\class Effect
\brief Base class for many of the effects in Audacity.

*//*******************************************************************/
#include "Effect.h"

#include <algorithm>

#include <wx/defs.h>
#include <wx/sizer.h>

#include "ConfigInterface.h"
#include "../ProjectSettings.h"
#include "../SelectFile.h"
#include "../ShuttleAutomation.h"
#include "../ShuttleGui.h"
#include "StatefulEffectUIServices.h"
#include "../SyncLock.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "wxFileNameWrapper.h"
#include "../widgets/ProgressDialog.h"
#include "../widgets/NumericTextCtrl.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/VetoDialogHook.h"

#include <unordered_map>

static const int kPlayID = 20102;
static_assert(kPlayID == EffectEditor::kPlayID);

using t2bHash = std::unordered_map< void*, bool >;

bool StatefulEffect::Instance::Process(EffectSettings &settings)
{
   return GetEffect().Process(*this, settings);
}

auto StatefulEffect::Instance::GetLatency(const EffectSettings &, double) const
   -> SampleCount
{
   return GetEffect().GetLatency().as_long_long();
}

size_t StatefulEffect::Instance::ProcessBlock(EffectSettings &,
   const float *const *, float *const *, size_t)
{
   return 0;
}

Effect::Effect()
{
}

Effect::~Effect()
{
}

// ComponentInterface implementation

PluginPath Effect::GetPath() const
{
   return BUILTIN_EFFECT_PREFIX + GetSymbol().Internal();
}

ComponentInterfaceSymbol Effect::GetSymbol() const
{
   return {};
}

VendorSymbol Effect::GetVendor() const
{
   return XO("Audacity");
}

wxString Effect::GetVersion() const
{
   return AUDACITY_VERSION_STRING;
}

TranslatableString Effect::GetDescription() const
{
   return {};
}

// EffectDefinitionInterface implementation

EffectType Effect::GetType() const
{
   return EffectTypeNone;
}

EffectFamilySymbol Effect::GetFamily() const
{
   // Unusually, the internal and visible strings differ for the built-in
   // effect family.
   return { wxT("Audacity"), XO("Built-in") };
}

bool Effect::IsInteractive() const
{
   return true;
}

bool Effect::IsDefault() const
{
   return true;
}

auto Effect::RealtimeSupport() const -> RealtimeSince
{
   return RealtimeSince::Never;
}

bool Effect::SupportsAutomation() const
{
   return true;
}

StatefulEffect::~StatefulEffect() = default;

std::shared_ptr<EffectInstance> StatefulEffect::MakeInstance() const
{
   // Cheat with const-cast to return an object that calls through to
   // non-const methods of a stateful effect.
   // Stateless effects should override this function and be really const
   // correct.
   return std::make_shared<Instance>(const_cast<StatefulEffect&>(*this));
}

std::unique_ptr<EffectEditor> StatefulEffect::PopulateUI(
   const EffectPlugin &, ShuttleGui &S,
   EffectInstance &instance, EffectSettingsAccess &access,
   const EffectOutputs *pOutputs) const
{
   auto parent = S.GetParent();

   // As in MakeInstance, we still cheat const for stateful effects!
   auto pThis = const_cast<StatefulEffect*>(this);

   // Let the effect subclass provide its own editor if it wants
   auto result = pThis->PopulateOrExchange(S, instance, access, pOutputs);

   parent->SetMinSize(parent->GetSizer()->GetMinSize());

   if (!result) {
      // No custom editor object?  Then use the default
      result = std::make_unique<DefaultEffectEditor>(*pThis,
         *pThis, access, S.GetParent());
      parent->PushEventHandler(pThis);
   }
   return result;
}

std::unique_ptr<EffectEditor> StatefulEffect::MakeEditor(
   ShuttleGui &, EffectInstance &, EffectSettingsAccess &,
   const EffectOutputs *) const
{
   assert(false);
   return nullptr;
}

const EffectParameterMethods &Effect::Parameters() const
{
   static const CapturedParameters<Effect> empty;
   return empty;
}

int Effect::ShowClientInterface(const EffectPlugin &, wxWindow &parent,
   wxDialog &dialog, EffectEditor *, bool forceModal) const
{
   dialog.Layout();
   dialog.Fit();
   dialog.SetMinSize(dialog.GetSize());
   if (VetoDialogHook::Call(&dialog))
      return 0;
   if (SupportsRealtime() && !forceModal) {
      dialog.Show();
      // Return false to bypass effect processing
      return 0;
   }
   return dialog.ShowModal();
}

int Effect::ShowHostInterface(wxWindow &parent,
   const EffectDialogFactory &factory,
   std::shared_ptr<EffectInstance> &pInstance, EffectSettingsAccess &access,
   bool forceModal)
{
   if (!IsInteractive())
      // Effect without UI just proceeds quietly to apply it destructively.
      return wxID_APPLY;

   // Create the dialog
   // Host, not client, is responsible for invoking the factory and managing
   // the lifetime of the dialog.
   // The usual factory lets the client (which is this, when self-hosting)
   // populate it.  That factory function is called indirectly through a
   // std::function to avoid source code dependency cycles.
   EffectUIServices *const client = this;
   auto results = factory(parent, *this, *client, access);
   auto pDialog = results.pDialog;
   pInstance = results.pInstance;
   if (!pDialog)
      return 0;

   // Let the client show the dialog and decide whether to keep it open
   auto result = client->ShowClientInterface(*this, parent, *pDialog,
      results.pEditor, forceModal);
   if (pDialog && !pDialog->IsShown())
      // Client didn't show it, or showed it modally and closed it
      // So destroy it.
      pDialog->Destroy();

   return result;
}

bool Effect::VisitSettings(SettingsVisitor &visitor, EffectSettings &settings)
{
   Parameters().Visit(*this, visitor, settings);
   return true;
}

bool Effect::VisitSettings(
   ConstSettingsVisitor &visitor, const EffectSettings &settings) const
{
   Parameters().Visit(*this, visitor, settings);
   return true;
}

bool Effect::SaveSettings(
   const EffectSettings &settings, CommandParameters & parms) const
{
   Parameters().Get( *this, settings, parms );
   return true;
}

bool Effect::LoadSettings(
   const CommandParameters & parms, EffectSettings &settings) const
{
   // The first argument, and with it the const_cast, will disappear when
   // all built-in effects are stateless.
   return Parameters().Set( *const_cast<Effect*>(this), parms, settings );
}

OptionalMessage Effect::LoadUserPreset(
   const RegistryPath & name, EffectSettings &settings) const
{
   // Find one string in the registry and then reinterpret it
   // as complete settings
   wxString parms;
   if (!GetConfig(GetDefinition(), PluginSettings::Private,
      name, wxT("Parameters"), parms))
      return {};

   return LoadSettingsFromString(parms, settings);
}

bool Effect::SaveUserPreset(
   const RegistryPath & name, const EffectSettings &settings) const
{
   // Save all settings as a single string value in the registry
   wxString parms;
   if (!SaveSettingsAsString(settings, parms))
      return false;

   return SetConfig(GetDefinition(), PluginSettings::Private,
      name, wxT("Parameters"), parms);
}

RegistryPaths Effect::GetFactoryPresets() const
{
   return {};
}

OptionalMessage Effect::LoadFactoryPreset(int id, EffectSettings &settings) const
{
   return { nullptr };
}

OptionalMessage Effect::LoadFactoryDefaults(EffectSettings &settings) const
{
   return LoadUserPreset(FactoryDefaultsGroup(), settings);
}

std::unique_ptr<EffectEditor> Effect::PopulateUI(const EffectPlugin &,
   ShuttleGui &S, EffectInstance &instance, EffectSettingsAccess &access,
   const EffectOutputs *pOutputs) const
{
   auto parent = S.GetParent();

   // Subclass must provide something
   auto result = MakeEditor(S, instance, access, pOutputs);
   assert(result);

   parent->SetMinSize(parent->GetSizer()->GetMinSize());

   return result;
}

bool Effect::ValidateUI(const EffectPlugin &context, EffectSettings &) const
{
   return true;
}

bool Effect::CloseUI() const
{
   return true;
}

bool Effect::CanExportPresets() const
{
   return true;
}

static const FileNames::FileTypes &PresetTypes()
{
   static const FileNames::FileTypes result {
      { XO("Presets"), { wxT("txt") }, true },
      FileNames::AllFiles
   };
   return result;
};

void Effect::ExportPresets(
   const EffectPlugin &, const EffectSettings &settings) const
{
   wxString params;
   SaveSettingsAsString(settings, params);
   auto commandId = GetSquashedName(GetSymbol().Internal());
   params =  commandId.GET() + ":" + params;

   auto path = SelectFile(FileNames::Operation::Presets,
                                     XO("Export Effect Parameters"),
                                     wxEmptyString,
                                     wxEmptyString,
                                     wxEmptyString,
                                     PresetTypes(),
                                     wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
                                     nullptr);
   if (path.empty()) {
      return;
   }

   // Create/Open the file
   wxFFile f(path, wxT("wb"));
   if (!f.IsOpened())
   {
      AudacityMessageBox(
         XO("Could not open file: \"%s\"").Format( path ),
         XO("Error Saving Effect Presets"),
         wxICON_EXCLAMATION,
         NULL);
      return;
   }

   f.Write(params);
   if (f.Error())
   {
      AudacityMessageBox(
         XO("Error writing to file: \"%s\"").Format( path ),
         XO("Error Saving Effect Presets"),
         wxICON_EXCLAMATION,
         NULL);
   }

   f.Close();


   //SetWindowTitle();

}

OptionalMessage Effect::ImportPresets(
   const EffectPlugin &, EffectSettings &settings) const
{
   wxString params;

   auto path = SelectFile(FileNames::Operation::Presets,
                                     XO("Import Effect Parameters"),
                                     wxEmptyString,
                                     wxEmptyString,
                                     wxEmptyString,
                                     PresetTypes(),
                                     wxFD_OPEN | wxRESIZE_BORDER,
                                     nullptr);
   if (path.empty()) {
      return {};
   }

   wxFFile f(path);
   if (!f.IsOpened())
      return {};

   OptionalMessage result{};

   if (f.ReadAll(&params)) {
      wxString ident = params.BeforeFirst(':');
      params = params.AfterFirst(':');

      auto commandId = GetSquashedName(GetSymbol().Internal());

      if (ident != commandId) {
         // effect identifiers are a sensible length!
         // must also have some params.
         if ((params.Length() < 2 ) || (ident.Length() < 2) || (ident.Length() > 30))
         {
            Effect::MessageBox(
               /* i18n-hint %s will be replaced by a file name */
               XO("%s: is not a valid presets file.\n")
               .Format(wxFileNameFromPath(path)));
         }
         else
         {
            Effect::MessageBox(
               /* i18n-hint %s will be replaced by a file name */
               XO("%s: is for a different Effect, Generator or Analyzer.\n")
               .Format(wxFileNameFromPath(path)));
         }
         return {};
      }
      result = LoadSettingsFromString(params, settings);
   }

   //SetWindowTitle();

   return result;
}

bool Effect::HasOptions() const
{
   return false;
}

void Effect::ShowOptions(const EffectPlugin &) const
{
}

// EffectPlugin implementation

const EffectSettingsManager& Effect::GetDefinition() const
{
   return *this;
}

NumericFormatSymbol Effect::GetSelectionFormat()
{
   if( !IsBatchProcessing() && FindProject() )
      return ProjectSettings::Get( *FindProject() ).GetSelectionFormat();
   return NumericConverter::HoursMinsSecondsFormat();
}

wxString Effect::GetSavedStateGroup()
{
   return wxT("SavedState");
}

// Effect implementation

bool Effect::SaveSettingsAsString(
   const EffectSettings &settings, wxString & parms) const
{
   CommandParameters eap;
   ShuttleGetAutomation S;
   S.mpEap = &eap;
   if( VisitSettings( S, settings ) ){
      ;// got eap value using VisitSettings.
   }
   // Won't be needed in future
   else if (!SaveSettings(settings, eap))
   {
      return false;
   }

   return eap.GetParameters(parms);
}

OptionalMessage Effect::LoadSettingsFromString(
   const wxString & parms, EffectSettings &settings) const
{
   // If the string starts with one of certain significant substrings,
   // then the rest of the string is reinterpreted as part of a registry key,
   // and a user or factory preset is then loaded.
   // (Where did these prefixes come from?  See EffectPresetsDialog; and
   // ultimately the uses of it by EffectManager::GetPreset, which is used by
   // the macro management dialog)
   wxString preset = parms;
   OptionalMessage result;
   if (preset.StartsWith(kUserPresetIdent))
   {
      preset.Replace(kUserPresetIdent, wxEmptyString, false);
      result = LoadUserPreset(UserPresetsGroup(preset), settings);
   }
   else if (preset.StartsWith(kFactoryPresetIdent))
   {
      preset.Replace(kFactoryPresetIdent, wxEmptyString, false);
      auto presets = GetFactoryPresets();
      result = LoadFactoryPreset(
         make_iterator_range( presets ).index( preset ), settings );
   }
   else if (preset.StartsWith(kCurrentSettingsIdent))
   {
      preset.Replace(kCurrentSettingsIdent, wxEmptyString, false);
      result = LoadUserPreset(CurrentSettingsGroup(), settings);
   }
   else if (preset.StartsWith(kFactoryDefaultsIdent))
   {
      preset.Replace(kFactoryDefaultsIdent, wxEmptyString, false);
      result = LoadUserPreset(FactoryDefaultsGroup(), settings);
   }
   else
   {
      // If the string did not start with any of the significant substrings,
      // then use VisitSettings or LoadSettings to reinterpret it,
      // or use LoadSettings.
      // This interprets what was written by SaveSettings, above.
      CommandParameters eap(parms);
      ShuttleSetAutomation S;
      S.SetForValidating( &eap );
      // VisitSettings returns false if not defined for this effect.
      // To do: fix const_cast in use of VisitSettings
      if ( !const_cast<Effect*>(this)->VisitSettings(S, settings) ) {
         // the old method...
         if (LoadSettings(eap, settings))
            return { nullptr };
      }
      else if( !S.bOK )
         result = {};
      else{
         result = { nullptr };
         S.SetForWriting( &eap );
         const_cast<Effect*>(this)->VisitSettings(S, settings);
      }
   }

   if (!result)
   {
      Effect::MessageBox(
         XO("%s: Could not load settings below. Default settings will be used.\n\n%s")
            .Format( GetName(), preset ) );
      // We are using default settings and we still wish to continue.
      result = { nullptr };
      //return false;
   }
   return result;
}

unsigned Effect::TestUIFlags(unsigned mask) {
   return mask & mUIFlags;
}

bool Effect::IsBatchProcessing() const
{
   return mIsBatch;
}

void Effect::SetBatchProcessing()
{
   mIsBatch = true;
   // Save effect's internal state in a special registry path
   // just for this purpose
   // If effect is not stateful, this step doesn't really matter, and the
   // settings object is a dummy
   auto dummySettings = MakeSettings();
   SaveUserPreset(GetSavedStateGroup(), dummySettings);
}

void Effect::UnsetBatchProcessing()
{
   mIsBatch = false;
   // Restore effect's internal state from registry
   // If effect is not stateful, this call doesn't really matter, and the
   // settings object is a dummy
   auto dummySettings = MakeSettings();
   // Ignore failure
   (void ) LoadUserPreset(GetSavedStateGroup(), dummySettings);
}

bool Effect::Delegate(Effect &delegate, EffectSettings &settings)
{
   NotifyingSelectedRegion region;
   region.setTimes( mT0, mT1 );

   return delegate.DoEffect(settings, mProjectRate, mTracks, mFactory,
      region, mUIFlags, nullptr, nullptr, nullptr);
}

bool Effect::TransferDataToWindow(const EffectSettings &)
{
   return true;
}

bool Effect::TransferDataFromWindow(EffectSettings &)
{
   return true;
}

bool Effect::TotalProgress(double frac, const TranslatableString &msg) const
{
   auto updateResult = (mProgress ?
      mProgress->Poll(frac * 1000, 1000, msg) :
      ProgressResult::Success);
   return (updateResult != ProgressResult::Success);
}

bool Effect::TrackProgress(
   int whichTrack, double frac, const TranslatableString &msg) const
{
   auto updateResult = (mProgress ?
      mProgress->Poll((whichTrack + frac) * 1000,
         (double) mNumTracks * 1000, msg) :
      ProgressResult::Success);
   return (updateResult != ProgressResult::Success);
}

bool Effect::TrackGroupProgress(
   int whichGroup, double frac, const TranslatableString &msg) const
{
   auto updateResult = (mProgress ?
      mProgress->Poll((whichGroup + frac) * 1000,
         (double) mNumGroups * 1000, msg) :
      ProgressResult::Success);
   return (updateResult != ProgressResult::Success);
}

void Effect::GetBounds(
   const WaveTrack &track, const WaveTrack *pRight,
   sampleCount *start, sampleCount *len)
{
   auto t0 = std::max( mT0, track.GetStartTime() );
   auto t1 = std::min( mT1, track.GetEndTime() );

   if ( pRight ) {
      t0 = std::min( t0, std::max( mT0, pRight->GetStartTime() ) );
      t1 = std::max( t1, std::min( mT1, pRight->GetEndTime() ) );
   }

   if (t1 > t0) {
      *start = track.TimeToLongSamples(t0);
      auto end = track.TimeToLongSamples(t1);
      *len = end - *start;
   }
   else {
      *start = 0;
      *len = 0;
   }
}

//
// private methods
//
// Use this method to copy the input tracks to mOutputTracks, if
// doing the processing on them, and replacing the originals only on success (and not cancel).
// Copy the group tracks that have tracks selected
// If not all sync-locked selected, then only selected wave tracks.
void Effect::CopyInputTracks(bool allSyncLockSelected)
{
   // Reset map
   mIMap.clear();
   mOMap.clear();

   mOutputTracks = TrackList::Create(
      const_cast<AudacityProject*>( FindProject() ) // how to remove this const_cast?
  );

   auto trackRange = mTracks->Any() +
      [&] (const Track *pTrack) {
         return allSyncLockSelected
         ? SyncLock::IsSelectedOrSyncLockSelected(pTrack)
         : track_cast<const WaveTrack*>( pTrack ) && pTrack->GetSelected();
      };

   t2bHash added;

   for (auto aTrack : trackRange)
   {
      Track *o = mOutputTracks->Add(aTrack->Duplicate());
      mIMap.push_back(aTrack);
      mOMap.push_back(o);
   }
}

Track *Effect::AddToOutputTracks(const std::shared_ptr<Track> &t)
{
   mIMap.push_back(NULL);
   mOMap.push_back(t.get());
   return mOutputTracks->Add(t);
}

bool Effect::CheckWhetherSkipEffect(const EffectSettings &) const
{
   return false;
}

double Effect::CalcPreviewInputLength(
   const EffectSettings &, double previewLength) const
{
   return previewLength;
}

int Effect::MessageBox( const TranslatableString& message,
   long style, const TranslatableString &titleStr) const
{
   auto title = titleStr.empty()
      ? GetName()
      : XO("%s: %s").Format( GetName(), titleStr );
   return AudacityMessageBox(message, title, style);
}

EffectUIServices* Effect::GetEffectUIServices()
{
   return this;
}
