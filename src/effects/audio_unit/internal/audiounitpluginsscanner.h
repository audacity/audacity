/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/effects_base/internal/au3/au3audiopluginscanner.h"

#include "au3-audio-unit/AudioUnitEffectsModule.h"

namespace au::effects {
class AudioUnitPluginsScanner : public Au3AudioPluginScanner
{
public:
    AudioUnitPluginsScanner()
        : Au3AudioPluginScanner(m_auModule) {}

private:
    ::AudioUnitEffectsModule m_auModule;
};
}
