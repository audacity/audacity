/*
* Audacity: A Digital Audio Editor
*/

#include "vst3pluginsmetareader.h"

#include "internal/vsttypes.h"

using namespace au::effects;

Vst3PluginsMetaReader::Vst3PluginsMetaReader()
    : Au3AudioPluginMetaReader{m_module}
{
}

bool Vst3PluginsMetaReader::canReadMeta(const muse::io::path_t& pluginPath) const
{
    return muse::io::suffix(pluginPath) == "vst3";
}

muse::audioplugins::AudioResourceType Vst3PluginsMetaReader::metaType() const
{
    return std::string(vst::AUDIO_RESOURCE_TYPE_NAME);
}
