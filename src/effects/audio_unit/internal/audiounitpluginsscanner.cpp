/*
* Audacity: A Digital Audio Editor
*/

#include "audiounitpluginsscanner.h"

#include "global/io/dir.h"

#include "libraries/lib-module-manager/PluginManager.h"
#include "libraries/lib-audio-unit/AudioUnitEffectsModule.h"

#include "au3wrap/internal/wxtypes_convert.h"

muse::io::paths_t au::effects::AudioUnitPluginsScanner::scanPlugins() const
{
    muse::io::paths_t result;

    ::AudioUnitEffectsModule auModule;
    PluginPaths paths = auModule.FindModulePaths(PluginManager::Get());

    for (const auto& path : paths) {
        const auto modulePath = path.BeforeFirst(';');
        result.emplace_back(muse::io::Dir::fromNativeSeparators(au3::wxToString(modulePath)));
    }

    return result;
}
