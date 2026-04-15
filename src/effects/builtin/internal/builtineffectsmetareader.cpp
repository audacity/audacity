/*
 * Audacity: A Digital Audio Editor
 */
#include "builtineffectsmetareader.h"

namespace au::effects {
BuiltinEffectsMetaReader::BuiltinEffectsMetaReader()
    : Au3AudioPluginMetaReader{m_builtinEffectsModule}
{
}

muse::audio::AudioResourceType BuiltinEffectsMetaReader::metaType() const
{
    return muse::audio::AudioResourceType::NativeEffect;
}

bool BuiltinEffectsMetaReader::canReadMeta(const muse::io::path_t& pluginPath) const
{
    return pluginPath.toStdString().find("Built-in Effect:") == 0;
}
}
