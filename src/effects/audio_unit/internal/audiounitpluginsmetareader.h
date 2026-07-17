/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/effects_base/internal/au3/au3audiopluginmetareader.h"
#include "au3-audio-unit/AudioUnitEffectsModule.h"

namespace au::effects {
class AudioUnitPluginsMetaReader : public Au3AudioPluginMetaReader
{
public:
    AudioUnitPluginsMetaReader();
    muse::audioplugins::PluginType metaType() const override;
    bool canReadMeta(const muse::io::path_t& path) const override;
private:
    ::AudioUnitEffectsModule m_module;
};
}
