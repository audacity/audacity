/*
* Audacity: A Digital Audio Editor
*/

#include "vst3pluginsmetareader.h"

using namespace au::effects;
using namespace muse;

Vst3PluginsMetaReader::Vst3PluginsMetaReader()
    : AbstractAudioPluginMetaReader{m_module}
{
}

bool Vst3PluginsMetaReader::canReadMeta(const io::path_t& pluginPath) const
{
    return io::suffix(pluginPath) == "vst3";
}

audio::AudioResourceType Vst3PluginsMetaReader::metaType() const
{
    return audio::AudioResourceType::VstPlugin;
}
