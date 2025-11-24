/*
* Audacity: A Digital Audio Editor
*/
#include "lv2pluginsscanner.h"

#include "global/io/dir.h"

#include "au3-module-manager/PluginManager.h"
#include "au3-lv2/LoadLV2.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "log.h"

using namespace muse;
using namespace au::effects;

muse::io::paths_t Lv2PluginsScanner::scanPlugins() const
{
    TRACEFUNC;

    io::paths_t result;

    LV2EffectsModule lv2;
    PluginPaths paths = lv2.FindModulePaths(PluginManager::Get());

    for (const auto& path : paths) {
        const auto modulePath = path.BeforeFirst(';');
        result.emplace_back(muse::io::Dir::fromNativeSeparators(au3::wxToString(modulePath)));
    }

    return result;
}
