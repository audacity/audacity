/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include "ui/internal/guiapplication.h"

#include "modularity/ioc.h"
#include "appshell/iappshellconfiguration.h"

#include "cmdoptions.h"

namespace au::appshell {
class SplashScreen;
}

namespace au::app {
class GuiApp : public muse::ui::GuiApplication
{
    muse::GlobalInject<appshell::IAppShellConfiguration> appshellConfiguration;

public:
    GuiApp(const std::shared_ptr<AudacityCmdOptions>& options);

private:
    std::shared_ptr<muse::CmdOptions> makeContextOptions(const muse::StringList& args) const override;
    QString mainWindowQmlPath(const QString& platform) const override;
    void showContextSplash(const muse::modularity::ContextPtr& ctxId) override;
    void doStartupScenario(const muse::modularity::ContextPtr& ctxId) override;
    void applyCommandLineOptions(const std::shared_ptr<muse::CmdOptions>& options) override;

    appshell::SplashScreen* m_splashScreen = nullptr;
};
}
