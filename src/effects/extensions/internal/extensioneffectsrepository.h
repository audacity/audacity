/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <vector>

#include "framework/extensions/extensionstypes.h"
#include "framework/extensions/iextensionsregister.h"
#include "framework/global/async/asyncable.h"
#include "framework/global/async/notification.h"
#include "framework/global/modularity/ioc.h"
#include "effects/effects_base/effectstypes.h"

#include "extensioneffecttypes.h"

namespace au::effects::extensions {
struct ExtensionEffectEntry {
    EffectId id;
    EffectDescriptor descriptor;
    muse::io::path_t bundlePath;
};

class ExtensionEffectsRepository : public muse::async::Asyncable
{
    muse::GlobalInject<muse::extensions::IExtensionsRegister> extensionsRegister;

public:
    //! Loads the enabled extensions' effects and keeps them in sync with the extensions register
    void init();
    //! Re-reads the enabled extensions; returns true if the effect list changed
    bool reload();
    muse::async::Notification changed() const;

    const std::vector<ExtensionEffectEntry>& effects() const;
    const ExtensionEffectEntry* effect(const EffectId& id) const;
    muse::io::paths_t pluginPaths() const;
    bool contains(const muse::io::path_t& path) const;

private:
    bool reload(const muse::extensions::ManifestList& manifests);

    bool m_initialized = false;
    std::vector<ExtensionEffectEntry> m_effects;
    muse::async::Notification m_changed;
};
} // namespace au::effects::extensions
