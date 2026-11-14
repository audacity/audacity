/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <optional>

#include "framework/audioplugins/iaudiopluginsscanner.h"
#include "framework/global/iapplication.h"
#include "framework/global/io/path.h"

#include "au3-strings/Identifier.h" // PluginPaths

class PluginProvider;
namespace BasicUI {
class ProgressDialog;
}

namespace au::effects {
class Au3AudioPluginScanner : public muse::audioplugins::IAudioPluginsScanner
{
public:

    Au3AudioPluginScanner(PluginProvider& provider);

    void init(muse::IApplication::RunMode mode);
    void deinit();

    muse::io::paths_t scanPlugins(muse::Progress* progress = nullptr) const override;

protected:
    virtual ::PluginPaths pluginPaths(BasicUI::ProgressDialog* progress) const;
    virtual muse::io::paths_t customPaths() const { return {}; }

private:
    virtual void doInit() {}
    void syncCustomPathsToProvider() const;

    PluginProvider& m_pluginProvider;
    std::optional<muse::IApplication::RunMode> m_runMode;
};
}
