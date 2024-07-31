/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "global/io/path.h"

#include "effects/effectstypes.h"

namespace au::effects {
class EffectsLoader
{
public:
    EffectsLoader() = default;

    ManifestList loadManifestList(const muse::io::path_t& path) const;

    Manifest parseManifest(const muse::ByteArray& data) const;

private:
    ManifestList manifestList(const muse::io::path_t& rootPath) const;
    muse::io::paths_t manifestPaths(const muse::io::path_t& rootPath) const;
    Manifest parseManifest(const muse::io::path_t& path) const;
    void resolvePaths(Manifest& manifest, const muse::io::path_t& rootDirPath) const;
};
}
