/*
* Audacity: A Digital Audio Editor
*/

#include "nyquistpluginsmetareader.h"

using namespace au::effects;
using namespace muse;

NyquistPluginsMetaReader::NyquistPluginsMetaReader()
    : Au3AudioPluginMetaReader{m_module}
{
}

bool NyquistPluginsMetaReader::canReadMeta(const io::path_t& pluginPath) const
{
    // Handle the special "Nyquist Prompt" plugin (which is not a file)
    if (pluginPath.toString() == "Nyquist Prompt") {
        return true;
    }

    // Handle regular .ny files
    return io::suffix(pluginPath) == "ny";
}

audio::AudioResourceType NyquistPluginsMetaReader::metaType() const
{
    return muse::audio::AudioResourceType::NyquistPlugin;
}
