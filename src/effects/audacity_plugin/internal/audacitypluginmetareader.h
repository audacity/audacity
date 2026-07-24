/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/audioplugins/iaudiopluginmetareader.h"
#include "framework/global/modularity/ioc.h"

#include "audacityplugin/iaudacitypluginhost.h"

namespace au::effects {
class AudacityPluginMetaReader final : public muse::audioplugins::IAudioPluginMetaReader
{
    muse::GlobalInject<au::audacityplugin::IAudacityPluginHost> audacityPluginHost;

public:
    muse::audioplugins::PluginType metaType() const override;
    bool canReadMeta(const muse::io::path_t& pluginPath) const override;
    muse::RetVal<muse::audioplugins::PluginMetaList> readMeta(const muse::io::path_t& pluginPath) const override;
};
} // namespace au::effects
