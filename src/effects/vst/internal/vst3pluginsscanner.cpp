/*
* Audacity: A Digital Audio Editor
*/

#include "vst3pluginsscanner.h"

#include "global/io/dir.h"

#include "libraries/lib-module-manager/PluginManager.h"
#include "libraries/lib-vst3/VST3EffectsModule.cpp"

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
