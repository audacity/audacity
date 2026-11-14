/*
 * Audacity: A Digital Audio Editor
 */
#include "nyquistpluginsscanner.h"

#include "au3-effects/EffectBase.h" // NYQUIST_PROMPT_ID

namespace au::effects {
::PluginPaths NyquistPluginsScanner::pluginPaths(BasicUI::ProgressDialog* progress) const
{
    auto paths = Au3AudioPluginScanner::pluginPaths(progress);
    paths.erase(
        std::remove(paths.begin(), paths.end(), NYQUIST_PROMPT_ID),
        paths.end());
    return paths;
}
}
