/*
 * Audacity: A Digital Audio Editor
 */
#include "builtineffectsscanner.h"

#include "effects/effects_base/internal/effectsutils.h"

namespace au::effects {
muse::io::paths_t BuiltinEffectsScanner::scanPlugins() const
{
    muse::io::paths_t paths;
    const auto metas = builtinEffectsRepository()->effectMetaList();
    for (const EffectMeta& meta : metas) {
        paths.push_back(meta.path);
    }
    return paths;
}
}
