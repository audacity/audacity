/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include <QStringList>

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/multiwindows/singleinstance.h"
#include "framework/ui/internal/guiapplication.h"

#include "appshell/iappshellconfiguration.h"

#include "cmdoptions.h"

namespace au::appshell {
class SplashScreen;
}

namespace au::app {
class GuiApp : public muse::ui::GuiApplication, public muse::async::Asyncable
{
    muse::GlobalInject<appshell::IAppShellConfiguration> appshellConfiguration;

public:
    GuiApp(const std::shared_ptr<AudacityCmdOptions>& options);

private:
    std::shared_ptr<muse::CmdOptions> makeContextOptions(const muse::StringList& args) const override;
    QString mainWindowQmlPath(const QString& platform) const override;
    void showContextSplash(const muse::modularity::ContextPtr& ctxId) override;
    void doSetup(const std::shared_ptr<muse::CmdOptions>& options) override;
    void doStartupScenario(const muse::modularity::ContextPtr& ctxId) override;
    void applyCommandLineOptions(const std::shared_ptr<muse::CmdOptions>& options) override;

    void onSecondInstanceArgs(const QStringList& args);

    appshell::SplashScreen* m_splashScreen = nullptr;
    muse::mi::SingleInstanceListener m_singleInstance;
};
}
