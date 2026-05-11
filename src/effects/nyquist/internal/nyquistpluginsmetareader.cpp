/*
* Audacity: A Digital Audio Editor
*/

#include "nyquistpluginsmetareader.h"

#include "nyquisttypes.h"

using namespace au::effects;
using namespace muse;

NyquistPluginsMetaReader::NyquistPluginsMetaReader()
    : Au3AudioPluginMetaReader{m_module}
{
}

bool NyquistPluginsMetaReader::canReadMeta(const io::path_t& pluginPath) const
{
    // Nyquist prompt is a special case, that we treat as a built-in effect.
    if (pluginPath.toString() == "Nyquist prompt") {
        return false;
    }

    // Handle regular .ny files
    return io::suffix(pluginPath) == "ny";
}

audioplugins::AudioResourceType NyquistPluginsMetaReader::metaType() const
{
    return std::string(nyquist::AUDIO_RESOURCE_TYPE_NAME);
}
