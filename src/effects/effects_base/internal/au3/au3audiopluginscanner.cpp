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
    doInit();
}

void Au3AudioPluginScanner::deinit()
{
    m_pluginProvider.Terminate();
}

muse::io::paths_t Au3AudioPluginScanner::scanPlugins() const
{
    muse::io::paths_t result;

    const PluginPaths paths = pluginPaths();
    for (const auto& path : paths) {
        const auto modulePath = path.BeforeFirst(';');
        auto convertedPath = muse::io::Dir::fromNativeSeparators(au3::wxToString(modulePath));
        result.emplace_back(std::move(convertedPath));
    }

    return result;
}

PluginPaths Au3AudioPluginScanner::pluginPaths() const
{
    return m_pluginProvider.FindModulePaths(PluginManager::Get());
}
}
