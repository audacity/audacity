/*
 * Audacity: A Digital Audio Editor
 */

#include "au3audiopluginscanner.h"

#include "au3-module-manager/PluginManager.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "framework/global/io/dir.h"

namespace au::effects {
Au3AudioPluginScanner::Au3AudioPluginScanner(PluginProvider& provider)
    : m_pluginProvider{provider}
{
}

void Au3AudioPluginScanner::init()
{
    m_pluginProvider.Initialize();
}

muse::io::paths_t Au3AudioPluginScanner::scanPlugins() const
{
    muse::io::paths_t result;

    const PluginPaths paths = m_pluginProvider.FindModulePaths(PluginManager::Get());

    // Simply return the paths - don't register plugins here
    // The AU4 framework will handle registration via subprocess validation
    for (const auto& path : paths) {
        const auto modulePath = path.BeforeFirst(';');
        auto convertedPath = muse::io::Dir::fromNativeSeparators(au3::wxToString(modulePath));
        result.emplace_back(std::move(convertedPath));
    }

    return result;
}
}
