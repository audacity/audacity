/*
* Audacity: A Digital Audio Editor
*/

#include "nyquistpluginsscanner.h"

#include "global/io/dir.h"

#include "au3-module-manager/PluginManager.h"
#include "au3-nyquist-effects/LoadNyquist.h"

#include "au3wrap/internal/wxtypes_convert.h"

using namespace muse;
using namespace au::effects;

muse::io::paths_t au::effects::NyquistPluginsScanner::scanPlugins() const
{
    TRACEFUNC;

    io::paths_t result;

    // Use the AU3 NyquistEffectsModule (in global namespace)
    ::NyquistEffectsModule nyquistModule;
    const PluginPaths paths = nyquistModule.FindModulePaths(PluginManager::Get());

    // Simply return the paths - don't register plugins here
    // The AU4 framework will handle registration via subprocess validation
    for (const auto& path : paths) {
        const auto modulePath = path.BeforeFirst(';');
        auto convertedPath = muse::io::Dir::fromNativeSeparators(au3::wxToString(modulePath));
        result.emplace_back(convertedPath);
    }

    return result;
}
