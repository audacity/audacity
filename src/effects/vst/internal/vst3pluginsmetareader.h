/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/effects_base/internal/au3/au3audiopluginmetareader.h"
#include "au3-vst3/VST3EffectsModule.h"

namespace au::effects {
class Vst3PluginsMetaReader : public Au3AudioPluginMetaReader
{
public:
    Vst3PluginsMetaReader();
    muse::audio::AudioResourceType metaType() const override;
    bool canReadMeta(const muse::io::path_t& pluginPath) const override;

private:
    VST3EffectsModule m_module;
};
}
