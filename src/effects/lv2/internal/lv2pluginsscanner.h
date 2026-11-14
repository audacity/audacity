/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "modularity/ioc.h"

#include "effects/effects_base/internal/au3/au3audiopluginscanner.h"
#include "effects/effects_base/ieffectsconfiguration.h"

#include "au3-lv2/LoadLV2.h"
#include "au3-module-manager/PluginManager.h"

namespace au::effects {
class Lv2PluginsScanner : public Au3AudioPluginScanner
{
    muse::GlobalInject<effects::IEffectsConfiguration> effectsConfiguration;

public:
    Lv2PluginsScanner()
        : Au3AudioPluginScanner(m_lv2Module) {}

protected:
    muse::io::paths_t customPaths() const override
    {
        return effectsConfiguration()->lv2CustomPaths();
    }

private:
    void doInit() override
    {
        // This won't actually register the LV2 plugins on the manager
        // but is needed to load the plugins in gWorld.
        m_lv2Module.AutoRegisterPlugins(::PluginManager::Get());
    }

    ::LV2EffectsModule m_lv2Module;
};
}
