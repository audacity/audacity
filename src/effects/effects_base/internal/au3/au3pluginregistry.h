/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-module-manager/IPluginRegistry.h"

#include "framework/audioplugins/iknownaudiopluginsregister.h"
#include "framework/audioplugins/iaudiopluginsconfiguration.h"
#include "framework/global/io/ifilesystem.h"
#include "framework/global/modularity/ioc.h"

namespace au::effects {
class Au3PluginRegistry : public ::IPluginRegistry
{
    muse::GlobalInject<muse::audioplugins::IKnownAudioPluginsRegister> knownPlugins;
    muse::GlobalInject<muse::audioplugins::IAudioPluginsConfiguration> audioPluginsConfiguration;
    muse::GlobalInject<muse::io::IFileSystem> fileSystem;

public:
    void Load(::PluginMap&) override;
    void Save(const ::PluginMap&, bool overwrite) override;
};
}
