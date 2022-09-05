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

#include "ConfigInterface.h"
#include "../LabelTrack.h"
#include "../ProjectSettings.h"
#include "../ShuttleAutomation.h"
#include "StatefulEffectUIServices.h"
#include "../SyncLock.h"
#include "ViewInfo.h"
#include "../WaveTrack.h"
#include "wxFileNameWrapper.h"
#include "../widgets/ProgressDialog.h"
#include "../widgets/NumericTextCtrl.h"
#include "../widgets/AudacityMessageBox.h"

#include <unordered_map>

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

const EffectParameterMethods &Effect::Parameters() const
{
   static const CapturedParameters<Effect> empty;
   return empty;
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

bool Effect::CanExportPresets() const
{
   return true;
}

bool Effect::HasOptions() const
{
   return false;
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

bool Effect::Delegate(
   EffectContext &context, Effect &delegate, EffectSettings &settings)
{
   NotifyingSelectedRegion region;
   region.setTimes( mT0, mT1 );

   return delegate.DoEffect(context, settings, {},
      mProjectRate, mTracks, region, nullptr);
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

Effect::AddedAnalysisTrack::AddedAnalysisTrack(Effect *pEffect, const wxString &name)
   : mpEffect(pEffect)
{
   if(!name.empty())
      mpTrack = LabelTrack::Create(*pEffect->mTracks, name);
   else
      mpTrack = LabelTrack::Create(*pEffect->mTracks);
}

Effect::AddedAnalysisTrack::AddedAnalysisTrack(AddedAnalysisTrack &&that)
{
   mpEffect = that.mpEffect;
   mpTrack = that.mpTrack;
   that.Commit();
}

void Effect::AddedAnalysisTrack::Commit()
{
   mpEffect = nullptr;
}

Effect::AddedAnalysisTrack::~AddedAnalysisTrack()
{
   if (mpEffect) {
      // not committed -- DELETE the label track
      mpEffect->mTracks->Remove(mpTrack);
   }
}

auto Effect::AddAnalysisTrack(const wxString &name) -> std::shared_ptr<AddedAnalysisTrack>
{
   return std::shared_ptr<AddedAnalysisTrack>
      { safenew AddedAnalysisTrack{ this, name } };
}

Effect::ModifiedAnalysisTrack::ModifiedAnalysisTrack
   (Effect *pEffect, const LabelTrack *pOrigTrack, const wxString &name)
   : mpEffect(pEffect)
{
   // copy LabelTrack here, so it can be undone on cancel
   auto newTrack = pOrigTrack->Copy(pOrigTrack->GetStartTime(), pOrigTrack->GetEndTime());

   mpTrack = static_cast<LabelTrack*>(newTrack.get());

   // Why doesn't LabelTrack::Copy complete the job? :
   mpTrack->SetOffset(pOrigTrack->GetStartTime());
   if (!name.empty())
      mpTrack->SetName(name);

   // mpOrigTrack came from mTracks which we own but expose as const to subclasses
   // So it's okay that we cast it back to const
   mpOrigTrack =
      pEffect->mTracks->Replace(const_cast<LabelTrack*>(pOrigTrack),
         newTrack );
}

Effect::ModifiedAnalysisTrack::ModifiedAnalysisTrack(ModifiedAnalysisTrack &&that)
{
   mpEffect = that.mpEffect;
   mpTrack = that.mpTrack;
   mpOrigTrack = std::move(that.mpOrigTrack);
   that.Commit();
}

void Effect::ModifiedAnalysisTrack::Commit()
{
   mpEffect = nullptr;
}

Effect::ModifiedAnalysisTrack::~ModifiedAnalysisTrack()
{
   if (mpEffect) {
      // not committed -- DELETE the label track
      // mpOrigTrack came from mTracks which we own but expose as const to subclasses
      // So it's okay that we cast it back to const
      mpEffect->mTracks->Replace(mpTrack, mpOrigTrack);
   }
}

auto Effect::ModifyAnalysisTrack
   (const LabelTrack *pOrigTrack, const wxString &name) -> ModifiedAnalysisTrack
{
   return{ this, pOrigTrack, name };
}

bool Effect::CheckWhetherSkipEffect(const EffectSettings &) const
{
   return false;
}

double Effect::CalcPreviewInputLength(const EffectContext &,
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
