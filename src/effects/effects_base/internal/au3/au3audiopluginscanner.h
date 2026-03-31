/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/audioplugins/iaudiopluginsscanner.h"

#include "au3-strings/Identifier.h" // PluginPaths

class PluginProvider;

namespace au::effects {
class Au3AudioPluginScanner : public muse::audioplugins::IAudioPluginsScanner
{
public:

    Au3AudioPluginScanner(PluginProvider& provider);

    void init();

    muse::io::paths_t scanPlugins() const override;

protected:
    virtual ::PluginPaths pluginPaths() const;

private:
    virtual void doInit() {}
    PluginProvider& m_pluginProvider;
};
}
