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
#include "EffectOutputTracks.h"

#include <algorithm>

#include <wx/defs.h>

#include "BasicUI.h"
#include "ConfigInterface.h"
#include "ProjectNumericFormats.h"
#include "ShuttleAutomation.h"
#include "SyncLock.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "wxFileNameWrapper.h"
#include "NumericConverterFormats.h"

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

const EffectParameterMethods& Effect::Parameters() const
{
    static const CapturedParameters<Effect> empty;
    return empty;
}

bool Effect::VisitSettings(SettingsVisitor& visitor, EffectSettings& settings)
{
    Parameters().Visit(*this, visitor, settings);
    return true;
}

bool Effect::VisitSettings(
    ConstSettingsVisitor& visitor, const EffectSettings& settings) const
{
    Parameters().Visit(*this, visitor, settings);
    return true;
}

bool Effect::SaveSettings(
    const EffectSettings& settings, CommandParameters& parms) const
{
    Parameters().Get(*this, settings, parms);
    return true;
}

bool Effect::LoadSettings(
    const CommandParameters& parms, EffectSettings& settings) const
{
    // The first argument, and with it the const_cast, will disappear when
    // all built-in effects are stateless.
    return Parameters().Set(*const_cast<Effect*>(this), parms, settings);
}

OptionalMessage Effect::LoadUserPreset(
    const RegistryPath& name, EffectSettings& settings) const
{
    // Find one string in the registry and then reinterpret it
    // as complete settings
    wxString parms;
    if (!GetConfig(GetDefinition(), PluginSettings::Private,
                   name, wxT("Parameters"), parms)) {
        return {};
    }

    return LoadSettingsFromString(parms, settings);
}

bool Effect::SaveUserPreset(
    const RegistryPath& name, const EffectSettings& settings) const
{
    // Save all settings as a single string value in the registry
    wxString parms;
    if (!SaveSettingsAsString(settings, parms)) {
        return false;
    }

    return SetConfig(GetDefinition(), PluginSettings::Private,
                     name, wxT("Parameters"), parms);
}

RegistryPaths Effect::GetFactoryPresets() const
{
    return {};
}

OptionalMessage Effect::LoadFactoryPreset(int id, EffectSettings& settings) const
{
    return { nullptr };
}

OptionalMessage Effect::LoadFactoryDefaults(EffectSettings& settings) const
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

NumericFormatID Effect::GetSelectionFormat()
{
    if (!IsBatchProcessing() && FindProject()) {
        return ProjectNumericFormats::Get(*FindProject())
               .GetSelectionFormat();
    }
    return NumericConverterFormats::HoursMinsSecondsFormat().Internal();
}

wxString Effect::GetSavedStateGroup()
{
    return wxT("SavedState");
}

// Effect implementation

bool Effect::SaveSettingsAsString(
    const EffectSettings& settings, wxString& parms) const
{
    CommandParameters eap;
    ShuttleGetAutomation S;
    S.mpEap = &eap;
    if (VisitSettings(S, settings)) {
        // got eap value using VisitSettings.
    }
    // Won't be needed in future
    else if (!SaveSettings(settings, eap)) {
        return false;
    }

    return eap.GetParameters(parms);
}

OptionalMessage Effect::LoadSettingsFromString(
    const wxString& parms, EffectSettings& settings) const
{
    // If the string starts with one of certain significant substrings,
    // then the rest of the string is reinterpreted as part of a registry key,
    // and a user or factory preset is then loaded.
    // (Where did these prefixes come from?  See EffectPresetsDialog; and
    // ultimately the uses of it by EffectManager::GetPreset, which is used by
    // the macro management dialog)
    wxString preset = parms;
    OptionalMessage result;
    if (preset.StartsWith(kUserPresetIdent)) {
        preset.Replace(kUserPresetIdent, wxEmptyString, false);
        result = LoadUserPreset(UserPresetsGroup(preset), settings);
    } else if (preset.StartsWith(kFactoryPresetIdent)) {
        preset.Replace(kFactoryPresetIdent, wxEmptyString, false);
        auto presets = GetFactoryPresets();
        result = LoadFactoryPreset(
            make_iterator_range(presets).index(preset), settings);
    } else if (preset.StartsWith(kCurrentSettingsIdent)) {
        preset.Replace(kCurrentSettingsIdent, wxEmptyString, false);
        result = LoadUserPreset(CurrentSettingsGroup(), settings);
    } else if (preset.StartsWith(kFactoryDefaultsIdent)) {
        preset.Replace(kFactoryDefaultsIdent, wxEmptyString, false);
        result = LoadUserPreset(FactoryDefaultsGroup(), settings);
    } else {
        // If the string did not start with any of the significant substrings,
        // then use VisitSettings or LoadSettings to reinterpret it,
        // or use LoadSettings.
        // This interprets what was written by SaveSettings, above.
        CommandParameters eap(parms);
        ShuttleSetAutomation S;
        S.SetForValidating(&eap);
        // VisitSettings returns false if not defined for this effect.
        // To do: fix const_cast in use of VisitSettings
        if (!const_cast<Effect*>(this)->VisitSettings(S, settings)) {
            // the old method...
            if (LoadSettings(eap, settings)) {
                return { nullptr };
            }
        } else if (!S.bOK) {
            result = {};
        } else {
            result = { nullptr };
            S.SetForWriting(&eap);
            const_cast<Effect*>(this)->VisitSettings(S, settings);
        }
    }

    if (!result) {
        using namespace BasicUI;
        ShowMessageBox(
            XO("%s: Could not load settings below. Default settings will be used.\n\n%s")
            .Format(GetName(), preset),
            MessageBoxOptions {}.Caption(GetName()));
        // We are using default settings and we still wish to continue.
        result = { nullptr };
    }
    return result;
}

unsigned Effect::TestUIFlags(unsigned mask)
{
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
    (void)LoadUserPreset(GetSavedStateGroup(), dummySettings);
}

bool Effect::Delegate(Effect& delegate, EffectSettings& settings,
                      InstanceFinder finder)
{
    if (!finder) {
        finder = DefaultInstanceFinder(delegate);
    }

    NotifyingSelectedRegion region;
    region.setTimes(mT0, mT1);

    return delegate.DoEffect(settings, finder, mProjectRate, mTracks.get(),
                             mFactory, region, mUIFlags, nullptr);
}

bool Effect::TotalProgress(double frac, const TranslatableString& msg) const
{
    auto updateResult = (mProgress
                         ? mProgress->Poll(frac * 1000, 1000, msg)
                         : BasicUI::ProgressResult::Success);
    return updateResult != BasicUI::ProgressResult::Success;
}

bool Effect::TrackProgress(
    int whichTrack, double frac, const TranslatableString& msg) const
{
    auto updateResult = (mProgress
                         ? mProgress->Poll((whichTrack + frac) * 1000,
                                           (double)mNumTracks * 1000, msg)
                         : BasicUI::ProgressResult::Success);
    return updateResult != BasicUI::ProgressResult::Success;
}

bool Effect::TrackGroupProgress(
    int whichGroup, double frac, const TranslatableString& msg) const
{
    auto updateResult = (mProgress
                         ? mProgress->Poll((whichGroup + frac) * 1000,
                                           (double)mNumGroups * 1000, msg)
                         : BasicUI::ProgressResult::Success);
    return updateResult != BasicUI::ProgressResult::Success;
}

void Effect::GetBounds(
    const WaveTrack& track, sampleCount* start, sampleCount* len)
{
    const auto t0 = std::max(mT0, track.GetStartTime());
    const auto t1 = std::min(mT1, track.GetEndTime());
    if (t1 > t0) {
        *start = track.TimeToLongSamples(t0);
        auto end = track.TimeToLongSamples(t1);
        *len = end - *start;
    } else {
        *start = 0;
        *len = 0;
    }
}

bool Effect::CheckWhetherSkipEffect(const EffectSettings&) const
{
    return false;
}

double Effect::CalcPreviewInputLength(
    const EffectSettings&, double previewLength) const
{
    return previewLength;
}
