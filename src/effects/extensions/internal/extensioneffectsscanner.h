/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include "framework/audioplugins/iknownaudiopluginsregister.h"
#include "framework/audioplugins/iaudiopluginsscanner.h"
#include "framework/global/modularity/ioc.h"

namespace muse::audioplugins {
class IRegisterAudioPluginsScenario;
}

namespace muse::extensions {
class IExtensionsProvider;
}

namespace au::effects::extensions {
class ExtensionEffectLoader;
class ExtensionEffectsRepository;

class ExtensionEffectsScanner final : public muse::audioplugins::IAudioPluginsScanner
{
public:
    ExtensionEffectsScanner(std::shared_ptr<muse::extensions::IExtensionsProvider> extensionsProvider,
                            std::shared_ptr<ExtensionEffectsRepository> repository, std::shared_ptr<ExtensionEffectLoader> effectLoader);

    muse::io::paths_t scanPlugins(muse::Progress* = nullptr) const override;
    void refreshPlugins(muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario) const;

private:
    muse::io::paths_t updateRepository() const;

    muse::GlobalInject<muse::audioplugins::IKnownAudioPluginsRegister> knownPlugins;

    std::shared_ptr<muse::extensions::IExtensionsProvider> m_extensionsProvider;
    std::shared_ptr<ExtensionEffectsRepository> m_repository;
    std::shared_ptr<ExtensionEffectLoader> m_effectLoader;
};
} // namespace au::effects::extensions
