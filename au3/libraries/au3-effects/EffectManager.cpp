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
#include "au3-module-manager/ConfigInterface.h"
#include "Effect.h"
#include "au3-module-manager/PluginManager.h"
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

EffectPlugin* EffectManager::GetEffect(const PluginID& ID, const EffectLoader& effectLoader)
{
    return DoGetEffect(ID, effectLoader).effect;
}

EffectSettings* EffectManager::GetDefaultSettings(const PluginID& ID, const EffectLoader& effectLoader)
{
    return GetEffectAndDefaultSettings(ID, effectLoader).second;
}

std::pair<EffectPlugin*, EffectSettings*>
EffectManager::GetEffectAndDefaultSettings(const PluginID& ID, const EffectLoader& effectLoader)
{
    auto& results = DoGetEffect(ID, effectLoader);
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
LoadComponent(EffectSettingsManager* manager)
{
    if (manager) {
        auto settings = manager->MakeSettings();
        InitializePreset(*manager, settings);
        return { manager, std::move(settings) };
    }
    return { nullptr, {} };
}
}

EffectAndDefaultSettings& EffectManager::DoGetEffect(const PluginID& ID, const EffectLoader& effectLoader)
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
        auto [component, settings] = LoadComponent(effectLoader(ID));
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
EffectManager::GetInstanceFactory(const PluginID& ID, const EffectLoader& effectLoader)
{
    return Get().GetEffect(ID, effectLoader);
}
