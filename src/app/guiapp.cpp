/*
 * Audacity: A Digital Audio Editor
 */
#include "guiapp.h"
#include "iapplication.h"
#include "thirdparty/kors_logger/src/log_base.h"

#include <QApplication>
#include <QQmlApplicationEngine>
#include <QQuickWindow>
#include <QQmlContext>
#include <QStyleHints>
#include <memory>
#ifndef Q_OS_WASM
#include <QThreadPool>
#endif

#include "modularity/ioc.h"
#include "async/processevents.h"
#include "ui/internal/uiengine.h"
#include "ui/graphicsapiprovider.h"

#include "appshell/internal/splashscreen/splashscreen.h"
#include "project/types/projecttypes.h"

#include "log.h"

using namespace muse;
using namespace au::app;
using namespace au::appshell;
namespace project = au::project;

GuiApp::GuiApp(const CommandLineParser::Options& options, const muse::modularity::ContextPtr& ctx)
    : BaseApplication(ctx), m_options(options)
{
    setRunMode(IApplication::RunMode::GuiApp);
}

int GuiApp::lastId()
{
    // TODO: should be provided by AppFactory
    static int lastId = -1;
#ifdef MUSE_MULTICONTEXT_WIP
    lastId++;
#endif
    return lastId;
}

void GuiApp::addModule(modularity::IModuleSetup* module)
{
    m_modules.push_back(module);
}

void GuiApp::setup()
{
    // ====================================================
    // Setup modules: Resources, Exports, Imports, UiTypes
    // ====================================================
    m_globalModule.setApplication(shared_from_this());
    m_globalModule.registerResources();
    m_globalModule.registerExports();
    m_globalModule.registerUiTypes();

    for (modularity::IModuleSetup* m : m_modules) {
        m->setApplication(shared_from_this());
        m->registerResources();
    }

    for (modularity::IModuleSetup* m : m_modules) {
        m->registerExports();
    }

#ifndef MUSE_MULTICONTEXT_WIP
    modularity::ContextPtr ctx = std::make_shared<modularity::Context>();
    ctx->id = 0;
    std::vector<muse::modularity::IContextSetup*>& csetups = contextSetups(ctx);
    for (modularity::IContextSetup* s : csetups) {
        s->registerExports();
    }
#endif

    m_globalModule.resolveImports();
    m_globalModule.registerApi();
    for (modularity::IModuleSetup* m : m_modules) {
        m->registerUiTypes();
        m->resolveImports();
        m->registerApi();
    }

#ifndef MUSE_MULTICONTEXT_WIP
    for (modularity::IContextSetup* s : csetups) {
        s->resolveImports();
    }
#endif

    applyCommandLineOptions(m_options);

    m_globalModule.onPreInit(runMode());
    for (modularity::IModuleSetup* m : m_modules) {
        m->onPreInit(runMode());
    }

#ifndef MUSE_MULTICONTEXT_WIP
    for (modularity::IContextSetup* s : csetups) {
        s->onPreInit(runMode());
    }
#endif

    m_splashScreen = new SplashScreen(iocContext(), SplashScreen::Default);
    m_splashScreen->show();

    m_globalModule.onInit(runMode());
    for (modularity::IModuleSetup* m : m_modules) {
        m->onInit(runMode());
    }

#ifndef MUSE_MULTICONTEXT_WIP
    for (modularity::IContextSetup* s : csetups) {
        s->onInit(runMode());
    }
#endif

    m_globalModule.onAllInited(runMode());
    for (modularity::IModuleSetup* m : m_modules) {
        m->onAllInited(runMode());
    }

#ifndef MUSE_MULTICONTEXT_WIP
    for (modularity::IContextSetup* s : csetups) {
        s->onAllInited(runMode());
    }
#endif

    QMetaObject::invokeMethod(qApp, [this]() {
        m_globalModule.onStartApp();
        for (modularity::IModuleSetup* m : m_modules) {
            m->onStartApp();
        }
    }, Qt::QueuedConnection);

    QQmlApplicationEngine* engine = modularity::ioc()->resolve<muse::ui::IUiEngine>("app")->qmlAppEngine();

    QObject::connect(engine, &QQmlApplicationEngine::objectCreated,
                     qApp, [this](QObject* obj, const QUrl& objUrl) {
        if (!obj) {
            LOGE() << "failed Qml load\n";
            QCoreApplication::exit(-1);
            return;
        }

        startupScenario()->runOnSplashScreen();

        m_globalModule.onDelayedInit();
        for (modularity::IModuleSetup* m : m_modules) {
            m->onDelayedInit();
        }

        if (m_splashScreen) {
            m_splashScreen->close();
            delete m_splashScreen;
            m_splashScreen = nullptr;
        }

        // The main window must be shown at this point so KDDockWidgets can read its size correctly
        // and scale all sizes properly. https://github.com/musescore/MuseScore/issues/21148
        QQuickWindow* w = dynamic_cast<QQuickWindow*>(obj);
        w->setVisible(true);

        startupScenario()->runAfterSplashScreen();
    }, Qt::QueuedConnection);

    QObject::connect(engine, &QQmlEngine::warnings, [](const QList<QQmlError>& warnings) {
        for (const QQmlError& e : warnings) {
            LOGE() << "error: " << e.toString().toStdString() << "\n";
        }
    });

    //! Needs to be set because we use transparent windows for PopupView.
    //! Needs to be called before any QQuickWindows are shown.
    QQuickWindow::setDefaultAlphaBuffer(true);

    {
        muse::ui::GraphicsApiProvider* gApiProvider = new muse::ui::GraphicsApiProvider(BaseApplication::appVersion());

        muse::ui::GraphicsApi required = gApiProvider->requiredGraphicsApi();
        if (required != muse::ui::GraphicsApi::Default) {
            LOGI() << "Setting required graphics api: " << muse::ui::GraphicsApiProvider::apiName(required);
            muse::ui::GraphicsApiProvider::setGraphicsApi(required);
        }

        LOGI() << "Using graphics api: " << muse::ui::GraphicsApiProvider::graphicsApiName();
        LOGI() << "Gui platform: " << QGuiApplication::platformName();

        if (muse::ui::GraphicsApiProvider::graphicsApi() == muse::ui::GraphicsApi::Software) {
            gApiProvider->destroy();
        } else {
            LOGI() << "Detecting problems with graphics api";
            gApiProvider->listen([this, gApiProvider, required](bool res) {
                if (res) {
                    LOGI() << "No problems detected with graphics api";
                    gApiProvider->setGraphicsApiStatus(required, muse::ui::GraphicsApiProvider::Status::Checked);
                } else {
                    muse::ui::GraphicsApi next = gApiProvider->switchToNextGraphicsApi(required);
                    LOGE() << "Detected problems with graphics api; switching from " << muse::ui::GraphicsApiProvider::apiName(required)
                           << " to " << muse::ui::GraphicsApiProvider::apiName(next);

                    this->restart();
                }
                gApiProvider->destroy();
            });
        }
    }
}

void GuiApp::finish()
{
    PROFILER_PRINT;

    // Wait Thread Pool
#ifndef Q_OS_WASM
    QThreadPool* globalThreadPool = QThreadPool::globalInstance();
    if (globalThreadPool) {
        LOGI() << "activeThreadCount: " << globalThreadPool->activeThreadCount();
        globalThreadPool->waitForDone();
    }
#endif

    // Engine quit
    modularity::ioc()->resolve<muse::ui::IUiEngine>("app")->quit();

    // Deinit
    muse::async::processMessages();
    for (modularity::IModuleSetup* m : m_modules) {
        m->onDeinit();
    }

    m_globalModule.onDeinit();

    for (modularity::IModuleSetup* m : m_modules) {
        m->onDestroy();
    }

    m_globalModule.onDestroy();

    // Delete contexts
    for (auto& c : m_contexts) {
        qDeleteAll(c.setups);
    }
    m_contexts.clear();

    // Delete modules
    qDeleteAll(m_modules);
    m_modules.clear();

    removeIoC();
}

std::vector<muse::modularity::IContextSetup*>& GuiApp::contextSetups(const muse::modularity::ContextPtr& ctx)
{
    for (Context& c : m_contexts) {
        if (c.ctx->id == ctx->id) {
            return c.setups;
        }
    }

    m_contexts.emplace_back();

    Context& ref = m_contexts.back();
    ref.ctx = ctx;

    modularity::IContextSetup* global = m_globalModule.newContext(ctx);
    if (global) {
        ref.setups.push_back(global);
    }

    for (modularity::IModuleSetup* m : m_modules) {
        modularity::IContextSetup* s = m->newContext(ctx);
        if (s) {
            ref.setups.push_back(s);
        }
    }

    return ref.setups;
}

modularity::ContextPtr GuiApp::setupNewContext()
{
#ifndef MUSE_MULTICONTEXT_WIP
    static bool once = false;
    IF_ASSERT_FAILED(!once) {
        return nullptr;
    }
    once = true;
#endif

    modularity::ContextPtr ctx = std::make_shared<modularity::Context>();
    ctx->id = lastId();

    LOGI() << "Setting up new context with id: " << ctx->id << "\n";

#ifdef MUSE_MULTICONTEXT_WIP
    std::vector<muse::modularity::IContextSetup*>& csetups = contextSetups(ctx);

    for (modularity::IContextSetup* s : csetups) {
        s->registerExports();
    }

    for (modularity::IContextSetup* s : csetups) {
        s->resolveImports();
    }

    for (modularity::IContextSetup* s : csetups) {
        s->onPreInit(runMode);
    }

    for (modularity::IContextSetup* s : csetups) {
        s->onInit(runMode);
    }

    for (modularity::IContextSetup* s : csetups) {
        s->onAllInited(runMode);
    }
#endif

    QQmlApplicationEngine* engine = muse::modularity::globalIoc()->resolve<muse::ui::IUiEngine>("app")->qmlAppEngine();
    QQmlComponent component(engine, "Audacity.AppShell", "Main");

    if (!component.isReady()) {
        LOGE() << "Failed to load Main.qml for new context: " << component.errorString().toStdString() << "\n";
        return nullptr;
    }

    QQmlContext* qmlCtx = new QQmlContext(engine);
    qmlCtx->setObjectName(QString("QQmlContext: %1").arg(ctx ? ctx->id : 0));
    QmlIoCContext* iocCtx = new QmlIoCContext(qmlCtx);
    iocCtx->ctx = ctx;
    qmlCtx->setContextProperty("ioc_context", QVariant::fromValue(iocCtx));

    QObject* obj = component.create(qmlCtx);
    if (!obj) {
        LOGE() << "Failed to create QML object for new context: " << component.errorString().toStdString() << "\n";
        QCoreApplication::exit(-1);
        return nullptr;
    }

    const auto finalizeStartup = [this, obj]() {
        static bool haveFinalized = false;
#ifndef MUSE_MULTICONTEXT_WIP
        IF_ASSERT_FAILED(!haveFinalized) {
            // Only call this once...
            return;
        }
#endif

        if (m_splashScreen) {
            m_splashScreen->close();
            delete m_splashScreen;
            m_splashScreen = nullptr;
        }

        // The main window must be shown at this point so KDDockWidgets can read its size correctly
        // and scale all sizes properly. https://github.com/musescore/MuseScore/issues/21148
        // but before that, let's make the window transparent,
        // otherwise the empty window frame will be visible
        // https://github.com/musescore/MuseScore/issues/29630
        // Transparency will be removed after the page loads.
        QQuickWindow* w = dynamic_cast<QQuickWindow*>(obj);
        w->setOpacity(0.01);
        w->setVisible(true);

        startupScenario()->runAfterSplashScreen();
        haveFinalized = true;
    };

    muse::async::Promise<Ret> promise = startupScenario()->runOnSplashScreen();
    promise.onResolve(nullptr, [finalizeStartup](Ret) {
        finalizeStartup();
    });

    return ctx;
}

int GuiApp::contextCount() const
{
    return static_cast<int>(m_contexts.size());
}

std::vector<modularity::ContextPtr> GuiApp::contexts() const
{
    std::vector<modularity::ContextPtr> ctxs;
    ctxs.reserve(m_contexts.size());
    for (const Context& c : m_contexts) {
        ctxs.push_back(c.ctx);
    }
    return ctxs;
}

void GuiApp::applyCommandLineOptions(const CommandLineParser::Options& options)
{
    if (options.app.revertToFactorySettings) {
        appshellConfiguration()->revertToFactorySettings(options.app.revertToFactorySettings.value());
    }

    startupScenario()->setStartupType(options.startup.type);

    if (options.startup.projectUrl.has_value()) {
        project::ProjectFile file { options.startup.projectUrl.value() };

        if (options.startup.projectDisplayNameOverride.has_value()) {
            file.displayNameOverride = options.startup.projectDisplayNameOverride.value();
        }

        startupScenario()->setStartupScoreFile(file);
    }

    if (options.app.loggerLevel) {
        m_globalModule.setLoggerLevel(options.app.loggerLevel.value());
    }
}
