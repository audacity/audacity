/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"

#include "effects/effects_base/internal/au3/au3audiopluginscanner.h"
#include "effects/effects_base/ieffectsconfiguration.h"

#include "au3-vst3/VST3EffectsModule.h"

namespace au::effects {
class Vst3PluginsScanner : public Au3AudioPluginScanner
{
    muse::GlobalInject<effects::IEffectsConfiguration> effectsConfiguration;

public:
    Vst3PluginsScanner()
        : Au3AudioPluginScanner(m_vst3Module) {}

protected:
    muse::io::paths_t customPaths() const override
    {
        return effectsConfiguration()->vst3CustomPaths();
    }

private:
    ::VST3EffectsModule m_vst3Module;
};
}
