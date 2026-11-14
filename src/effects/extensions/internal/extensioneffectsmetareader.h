/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include "framework/audioplugins/iknownaudiopluginsregister.h"
#include "framework/audioplugins/iaudiopluginmetareader.h"
#include "framework/global/modularity/ioc.h"

namespace au::effects::extensions {
class ExtensionEffectsRepository;

class ExtensionEffectsMetaReader final : public muse::audioplugins::IAudioPluginMetaReader
{
public:
    explicit ExtensionEffectsMetaReader(std::shared_ptr<ExtensionEffectsRepository> repository);

    muse::audioplugins::PluginType metaType() const override;
    bool canReadMeta(const muse::io::path_t& pluginPath) const override;
    muse::RetVal<muse::audioplugins::PluginMetaList> readMeta(const muse::io::path_t& pluginPath) const override;

private:
    muse::GlobalInject<muse::audioplugins::IKnownAudioPluginsRegister> knownPlugins;

    std::shared_ptr<ExtensionEffectsRepository> m_repository;
};
} // namespace au::effects::extensions
