/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/effects_base/internal/abstractaudiopluginmetareader.h"
#include "libraries/lib-vst3/VST3EffectsModule.h"

namespace au::effects {
class Vst3PluginsMetaReader : public AbstractAudioPluginMetaReader
{
public:
    Vst3PluginsMetaReader();
    muse::audio::AudioResourceType metaType() const override;
    bool canReadMeta(const muse::io::path_t& pluginPath) const override;

private:
    VST3EffectsModule m_module;
};
}
