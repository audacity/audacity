/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/audioplugins/iaudiopluginsscanner.h"

class PluginProvider;

namespace au::effects {
class Au3AudioPluginScanner : public muse::audioplugins::IAudioPluginsScanner
{
public:

    Au3AudioPluginScanner(PluginProvider& provider);

    void init();

    muse::io::paths_t scanPlugins() const override;

private:
    PluginProvider& m_pluginProvider;
};
}
