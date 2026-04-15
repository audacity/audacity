/*
* Audacity: A Digital Audio Editor
*/

#include "vst3pluginsscanner.h"

#include "global/io/dir.h"

#include "au3-module-manager/PluginManager.h"
#include "au3-vst3/VST3EffectsModule.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "log.h"

using namespace au::effects;
using namespace muse;

io::paths_t Vst3PluginsScanner::scanPlugins() const
{
    TRACEFUNC;

    io::paths_t result;

    VST3EffectsModule vst3Module;
    PluginPaths paths = vst3Module.FindModulePaths(PluginManager::Get());

    for (const auto& path : paths) {
        const auto modulePath = path.BeforeFirst(';');
        result.emplace_back(muse::io::Dir::fromNativeSeparators(au3::wxToString(modulePath)));
    }

    return result;
}
