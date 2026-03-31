/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/internal/au3effectloader.h"
#include "au3-lv2/LoadLV2.h"
#include "au3-module-manager/PluginManager.h"

namespace au::effects {
class Lv2EffectLoader final : public Au3EffectLoader
{
public:
    Lv2EffectLoader()
        : Au3EffectLoader(m_module, muse::audio::AudioResourceType::Lv2Plugin) {}

private:
    void doInit() override
    {
        // This won't actually register the LV2 plugins on the manager
        // but is needed to load the plugins in gWorld.
        m_module.AutoRegisterPlugins(::PluginManager::Get());
    }

    LV2EffectsModule m_module;
};
} // namespace au::effects
