/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "audioplugins/iaudiopluginmetareader.h"

namespace au::effects {
class Vst3PluginsMetaReader : public muse::audioplugins::IAudioPluginMetaReader
{
public:
    bool canReadMeta(const muse::io::path_t& pluginPath) const override;
    muse::RetVal<muse::audio::AudioResourceMetaList> readMeta(const muse::io::path_t& pluginPath) const override;
};
}
