/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include "framework/audioplugins/iknownaudiopluginsregister.h"
#include "framework/audioplugins/iaudiopluginsscanner.h"
#include "framework/extensions/iextensionsregister.h"
#include "framework/global/modularity/ioc.h"

namespace muse::audioplugins {
class IRegisterAudioPluginsScenario;
}

namespace au::effects::extensions {
class ExtensionEffectsRepository;

class ExtensionEffectsScanner final : public muse::audioplugins::IAudioPluginsScanner
{
public:
    explicit ExtensionEffectsScanner(std::shared_ptr<ExtensionEffectsRepository> repository);

    muse::io::paths_t scanPlugins(muse::Progress* = nullptr) const override;
    void refreshPlugins(muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario) const;

private:
    muse::io::paths_t updateRepository() const;

    muse::GlobalInject<muse::audioplugins::IKnownAudioPluginsRegister> knownPlugins;
    muse::GlobalInject<muse::extensions::IExtensionsRegister> extensionsRegister;

    std::shared_ptr<ExtensionEffectsRepository> m_repository;
};
} // namespace au::effects::extensions
