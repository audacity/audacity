/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <vector>

#include "framework/extensions/extensionstypes.h"
#include "effects/effects_base/effectstypes.h"

#include "extensioneffecttypes.h"

namespace au::effects::extensions {
struct ExtensionEffectEntry {
    EffectId id;
    EffectDescriptor descriptor;
    muse::io::path_t bundlePath;
};

class ExtensionEffectsRepository
{
public:
    void initialize(const muse::extensions::ManifestList& manifests);
    bool reload(const muse::extensions::ManifestList& manifests);

    const std::vector<ExtensionEffectEntry>& effects() const;
    const ExtensionEffectEntry* effect(const EffectId& id) const;
    muse::io::paths_t pluginPaths() const;
    bool contains(const muse::io::path_t& path) const;

private:
    bool m_initialized = false;
    std::vector<ExtensionEffectEntry> m_effects;
};
} // namespace au::effects::extensions
