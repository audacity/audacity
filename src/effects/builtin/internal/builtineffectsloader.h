/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/internal/au3effectloader.h"
#include "au3-effects/LoadEffects.h"
#include "au3-module-manager/PluginManager.h"

namespace au::effects {
class BuiltinEffectsLoader final : public Au3EffectLoader
{
public:
    BuiltinEffectsLoader()
        : Au3EffectLoader(m_module, muse::audio::AudioResourceType::NativeEffect) {}

private:
    void doInit() override
    {
        // Register the builtin effects with PluginManager so that
        // PluginManager::SettingsPath can build per-effect config paths.
        // Otherwise all builtin effects share a single "UserPresets/" group
        // in audacity.cfg, leaking user presets across effects.
        m_module.AutoRegisterPlugins(::PluginManager::Get());
    }

    ::BuiltinEffectsModule m_module;
};
} // namespace au::effects
