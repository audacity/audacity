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

void NyquistPluginsMetaReader::doInit(const IApplication::RunMode&)
{
    // Lazy initialization: if not initialized yet, initialize now
    // This is needed because the subprocess that registers plugins creates new instances
    // of meta readers that haven't been initialized via onInit()
    m_module.Initialize();
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
