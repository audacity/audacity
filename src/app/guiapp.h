/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>
#include <vector>

#include "modularity/imodulesetup.h"
#include "global/internal/baseapplication.h"
#include "global/globalmodule.h"
#include "modularity/ioc.h"
#include "appshell/istartupscenario.h"
#include "appshell/iappshellconfiguration.h"

#include "commandlineparser.h"

namespace au::appshell {
class SplashScreen;
}

namespace au::app {
class GuiApp : public muse::BaseApplication, public std::enable_shared_from_this<GuiApp>
{
    muse::GlobalInject<appshell::IAppShellConfiguration> appshellConfiguration;
    muse::Inject<appshell::IStartupScenario> startupScenario{ this };

public:
    GuiApp(const CommandLineParser::Options& options, const muse::modularity::ContextPtr& ctx);

    void addModule(muse::modularity::IModuleSetup* module);

    void setup() override;
    void finish() override;

    muse::modularity::ContextPtr setupNewContext() override;
    int contextCount() const override;
    std::vector<muse::modularity::ContextPtr> contexts() const override;

private:
    static int lastId();

    void applyCommandLineOptions(const CommandLineParser::Options& options);
    std::vector<muse::modularity::IContextSetup*>& contextSetups(const muse::modularity::ContextPtr& ctx);

    CommandLineParser::Options m_options;
    au::appshell::SplashScreen* m_splashScreen = nullptr;
    muse::GlobalModule m_globalModule;
    std::vector<muse::modularity::IModuleSetup*> m_modules;

    struct Context {
        muse::modularity::ContextPtr ctx;
        std::vector<muse::modularity::IContextSetup*> setups;
    };

    std::vector<Context> m_contexts;
};
}
