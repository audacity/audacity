/*
 * Audacity: A Digital Audio Editor
 */
#include "nyquistpluginsscanner.h"

#include "au3-module-manager/PluginManager.h" // NYQUIST_PROMPT_ID

namespace au::effects {
::PluginPaths NyquistPluginsScanner::pluginPaths() const
{
    auto paths = Au3AudioPluginScanner::pluginPaths();
    paths.erase(
        std::remove(paths.begin(), paths.end(), NYQUIST_PROMPT_ID),
        paths.end());
    return paths;
}
}
