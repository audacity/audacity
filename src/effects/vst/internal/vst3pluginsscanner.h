/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/effects_base/internal/au3/au3audiopluginscanner.h"

#include "au3-vst3/VST3EffectsModule.h"

namespace au::effects {
class Vst3PluginsScanner : public Au3AudioPluginScanner
{
public:
    Vst3PluginsScanner()
        : Au3AudioPluginScanner(m_vst3Module) {}

private:
    ::VST3EffectsModule m_vst3Module;
};
}
