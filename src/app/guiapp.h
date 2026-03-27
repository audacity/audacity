/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>
#include <vector>

#include <QPointer>
#include <QTimer>

#include "modularity/imodulesetup.h"
#include "global/internal/baseapplication.h"
#include "global/globalmodule.h"
#include "modularity/ioc.h"
#include "appshell/iappshellconfiguration.h"

#include "commandlineparser.h"

class QQuickWindow;

namespace au::appshell {
class SplashScreen;
}

namespace au::app {
class GuiApp : public muse::BaseApplication, public std::enable_shared_from_this<GuiApp>
{
    muse::GlobalInject<appshell::IAppShellConfiguration> appshellConfiguration;

public:
    GuiApp(const CommandLineParser::Options& options);

    void addModule(muse::modularity::IModuleSetup* module);

    void setup() override;
    void finish() override;

    muse::modularity::ContextPtr setupNewContext(const muse::StringList& args = {}) override;
    void destroyContext(const muse::modularity::ContextPtr& ctx) override;
    size_t contextCount() const override;
    std::vector<muse::modularity::ContextPtr> contexts() const override;

private:
    void applyCommandLineOptions(const CommandLineParser::Options& options);

    struct Context {
        muse::modularity::ContextPtr ctx;
        std::vector<muse::modularity::IContextSetup*> setups;
        QPointer<QQuickWindow> window;

        bool isValid() const { return ctx != nullptr && !setups.empty(); }
    };

    Context& context(const muse::modularity::ContextPtr& ctx);

    CommandLineParser::Options m_options;
    au::appshell::SplashScreen* m_splashScreen = nullptr;

    //! NOTE Separately to initialize logger and profiler as early as possible
    muse::GlobalModule* m_globalModule = nullptr;
    std::vector<muse::modularity::IModuleSetup*> m_modules;
    QTimer m_delayedInitTimer;

    std::vector<Context> m_contexts;
};
}
