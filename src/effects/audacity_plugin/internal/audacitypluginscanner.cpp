/*
 * Audacity: A Digital Audio Editor
 */
#include "audacitypluginscanner.h"

#include <set>

namespace au::effects {
muse::io::paths_t AudacityPluginScanner::scanPlugins(muse::Progress*) const
{
    if (!audacityPluginHost()) {
        return {};
    }

    std::set<std::string> uniquePaths;
    for (const auto& effect : audacityPluginHost()->effects()) {
        uniquePaths.insert(effect.bundlePath);
    }

    muse::io::paths_t result;
    result.reserve(uniquePaths.size());
    for (const auto& path : uniquePaths) {
        result.emplace_back(path);
    }
    return result;
}
} // namespace au::effects
