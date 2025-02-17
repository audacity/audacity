/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectManager.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

******************************************************************//**

\class EffectManager
\brief EffectManager is the class that handles effects and effect categories.

It maintains a graph of effect categories and subcategories,
registers and unregisters effects and can return filtered lists of
effects.

*//*******************************************************************/

#include "EffectManager.h"
#include "ConfigInterface.h"
#include "Effect.h"
#include "PluginManager.h"
#include <algorithm>

/*******************************************************************************
Creates a singleton and returns reference

 (Thread-safe...no active threading during construction or after destruction)
*******************************************************************************/
EffectManager& EffectManager::Get()
{
    static EffectManager em;
    return em;
}

// Here solely for the purpose of Nyquist Workbench until
// a better solution is devised.
const PluginID& EffectManager::RegisterEffect(
    std::unique_ptr<EffectPlugin> uEffect)
{
    auto pEffect = uEffect.get();
    const PluginID& ID
        =PluginManager::Get().RegisterPlugin(std::move(uEffect), PluginTypeEffect);
    mEffects[ID] = { pEffect, {} };
    return ID;
}

// Here solely for the purpose of Nyquist Workbench until
// a better solution is devised.
void EffectManager::UnregisterEffect(const PluginID& ID)
{
    PluginID id = ID;
    PluginManager::Get().UnregisterPlugin(id);
    mEffects.erase(id);
}

TranslatableString EffectManager::GetEffectFamilyName(const PluginID& ID)
{
    if (auto description = PluginManager::Get().GetPlugin(ID)) {
        return TranslatableString { description->GetEffectFamily(), {} };
    }

    auto effect = GetEffect(ID);
    if (effect) {
        return effect->GetDefinition().GetFamily().Msgid();
    }
    return {};
}

TranslatableString EffectManager::GetVendorName(const PluginID& ID)
{
    if (auto description = PluginManager::Get().GetPlugin(ID)) {
        return TranslatableString { description->GetVendor(), {} };
    }

    auto effect = GetEffect(ID);
    if (effect) {
        return effect->GetDefinition().GetVendor().Msgid();
    }
    return {};
}

bool EffectManager::IsHidden(const PluginID& ID)
{
    if (auto effect = GetEffect(ID)) {
        return effect->GetDefinition().IsHiddenFromMenus();
    }
    return false;
}

void EffectManager::SetSkipStateFlag(bool flag)
{
    mSkipStateFlag = flag;
}

bool EffectManager::GetSkipStateFlag()
{
    return mSkipStateFlag;
}

bool HasCurrentSettings(EffectPlugin& host)
{
    return HasConfigGroup(host.GetDefinition(), PluginSettings::Private,
                          CurrentSettingsGroup());
}

bool HasFactoryDefaults(EffectPlugin& host)
{
    return HasConfigGroup(host.GetDefinition(), PluginSettings::Private,
                          FactoryDefaultsGroup());
}

RegistryPaths GetUserPresets(EffectPlugin& host)
{
    RegistryPaths presets;
    GetConfigSubgroups(host.GetDefinition(), PluginSettings::Private,
                       UserPresetsGroup({}), presets);
    std::sort(presets.begin(), presets.end());
    return presets;
}

bool EffectManager::HasPresets(const PluginID& ID)
{
    auto effect = GetEffect(ID);

    if (!effect) {
        return false;
    }

    return GetUserPresets(*effect).size() > 0
           || effect->GetDefinition().GetFactoryPresets().size() > 0
           || HasCurrentSettings(*effect)
           || HasFactoryDefaults(*effect);
}

// This function is used only in the macro programming user interface
wxString EffectManager::GetPreset(
    const PluginID& ID, const wxString& params, EffectPresetDialog dialog)
{
    auto effect = GetEffect(ID);

    if (!effect) {
        return wxEmptyString;
    }

    CommandParameters eap(params);

    wxString preset;
    if (eap.HasEntry(wxT("Use Preset"))) {
        preset = eap.Read(wxT("Use Preset"));
    }

    if (const auto answer = dialog(*effect, preset)) {
        preset = *answer;
    } else {
        preset = wxEmptyString;
    }

    if (preset.empty()) {
        return preset;
    }

    // This cleans a config "file" backed by a string in memory.
    eap.DeleteAll();

    eap.Write(wxT("Use Preset"), preset);
    eap.GetParameters(preset);

    return preset;
}

// This function is used only in the macro programming user interface
wxString EffectManager::GetDefaultPreset(const PluginID& ID)
{
    auto effect = GetEffect(ID);

    if (!effect) {
        return wxEmptyString;
    }

    wxString preset;
    if (HasCurrentSettings(*effect)) {
        preset = EffectPlugin::kCurrentSettingsIdent;
    } else if (HasFactoryDefaults(*effect)) {
        preset = EffectPlugin::kFactoryDefaultsIdent;
    }

    if (!preset.empty()) {
        CommandParameters eap;

        eap.Write(wxT("Use Preset"), preset);
        eap.GetParameters(preset);
    }

    return preset;
}

EffectPlugin* EffectManager::GetEffect(const PluginID& ID)
{
    return DoGetEffect(ID).effect;
}

EffectSettings* EffectManager::GetDefaultSettings(const PluginID& ID)
{
    return GetEffectAndDefaultSettings(ID).second;
}

std::pair<EffectPlugin*, EffectSettings*>
EffectManager::GetEffectAndDefaultSettings(const PluginID& ID)
{
    auto& results = DoGetEffect(ID);
    if (results.effect) {
        return { results.effect, &results.settings };
    } else {
        return { nullptr, nullptr };
    }
}

namespace {
// Before: settings are as defaulted by `manager.MakeSettings()`
// Do as needed (once, persistently, when the plug-in is first used): store
// those default values into the config under "FactoryDefaults" preset
// After: settings are loaded for the "CurrentSettings" preset
void InitializePreset(
    EffectSettingsManager& manager, EffectSettings& settings)
{
    // Config key remembering whether we already stored FactoryDefaults
    constexpr auto InitializedKey = L"Initialized";
    if (bool haveDefaults{};
        GetConfig(manager, PluginSettings::Private, FactoryDefaultsGroup(),
                  InitializedKey, haveDefaults, false),
        !haveDefaults
        ) {
        manager.SaveUserPreset(FactoryDefaultsGroup(), settings);
        // Also initialize the "current" settings --
        if (bool haveCurrent{};
            GetConfig(manager, PluginSettings::Private, CurrentSettingsGroup(),
                      InitializedKey, haveCurrent, false),
            !haveCurrent
            ) {
            manager.SaveUserPreset(CurrentSettingsGroup(), settings);
        }
        SetConfig(manager, PluginSettings::Private, FactoryDefaultsGroup(),
                  InitializedKey, true);
    }
    // ignore failure
    (void)manager.LoadUserPreset(CurrentSettingsGroup(), settings);
}

std::pair<ComponentInterface*, EffectSettings>
LoadComponent(const PluginID& ID)
{
    if (auto result = dynamic_cast<EffectSettingsManager*>(
            PluginManager::Get().Load(ID))) {
        auto settings = result->MakeSettings();
        InitializePreset(*result, settings);
        return { result, std::move(settings) };
    }
    return { nullptr, {} };
}
}

EffectAndDefaultSettings& EffectManager::DoGetEffect(const PluginID& ID)
{
    static EffectAndDefaultSettings empty;

    // Must have a "valid" ID
    if (ID.empty()) {
        return empty;
    }

    if (auto iter = mEffects.find(ID); iter != mEffects.end()) {
        return iter->second;
    } else {
        // This will instantiate the effect client if it hasn't already been done
        auto [component, settings] = LoadComponent(ID);
        if (!component) {
            return empty;
        }

        if (auto effect = dynamic_cast<EffectPlugin*>(component)) {
            return mEffects[ID] = { effect, std::move(settings) };
        } else {
            return empty;
        }
    }
}

const EffectInstanceFactory*
EffectManager::GetInstanceFactory(const PluginID& ID)
{
    return Get().GetEffect(ID);
}
