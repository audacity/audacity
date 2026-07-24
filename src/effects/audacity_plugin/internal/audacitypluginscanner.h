/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/audioplugins/iaudiopluginsscanner.h"
#include "framework/global/modularity/ioc.h"

#include "audacityplugin/iaudacitypluginhost.h"

namespace au::effects {
class AudacityPluginScanner final : public muse::audioplugins::IAudioPluginsScanner
{
    muse::GlobalInject<au::audacityplugin::IAudacityPluginHost> audacityPluginHost;

public:
    muse::io::paths_t scanPlugins(muse::Progress* = nullptr) const override;
};
} // namespace au::effects
