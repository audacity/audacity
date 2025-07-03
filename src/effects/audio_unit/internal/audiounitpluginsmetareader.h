/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/effects_base/internal/abstractaudiopluginmetareader.h"
#include "libraries/lib-audio-unit/AudioUnitEffectsModule.h"

namespace au::effects {
class AudioUnitPluginsMetaReader : public AbstractAudioPluginMetaReader
{
public:
    AudioUnitPluginsMetaReader();
    muse::audio::AudioResourceType metaType() const override;
    bool canReadMeta(const muse::io::path_t& path) const override;
private:
    ::AudioUnitEffectsModule m_module;
};
}
