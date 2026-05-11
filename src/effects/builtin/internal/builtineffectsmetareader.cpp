/*
 * Audacity: A Digital Audio Editor
 */
#include "builtineffectsmetareader.h"
#include "builtintypes.h"

namespace au::effects {
BuiltinEffectsMetaReader::BuiltinEffectsMetaReader()
    : Au3AudioPluginMetaReader{m_builtinEffectsModule}
{
}

muse::audioplugins::AudioResourceType BuiltinEffectsMetaReader::metaType() const
{
    return std::string(au::effects::builtin::AUDIO_RESOURCE_TYPE_NAME);
}

bool BuiltinEffectsMetaReader::canReadMeta(const muse::io::path_t& pluginPath) const
{
    return pluginPath.toStdString().find("Built-in Effect:") == 0;
}
}
