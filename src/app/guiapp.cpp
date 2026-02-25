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
#include "ui/view/qmlapi.h"
#include "ui/graphicsapiprovider.h"

#include "appshell/internal/splashscreen/splashscreen.h"
#include "project/types/projecttypes.h"

#include "muse_framework_config.h"

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
    static int id = 0;

#ifdef MUSE_MULTICONTEXT_WIP
    return ++id;
#else
    return id;
#endif
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

    m_globalModule.onDelayedInit();
    for (modularity::IModuleSetup* m : m_modules) {
        m->onDelayedInit();
    }

    QMetaObject::invokeMethod(qApp, [this]() {
        m_globalModule.onStartApp();
        for (modularity::IModuleSetup* m : m_modules) {
            m->onStartApp();
        }
    }, Qt::QueuedConnection);

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

    // Deinit
    muse::async::processMessages();

    std::vector<modularity::ContextPtr> ctxs = contexts();
    for (auto& c : ctxs) {
        destroyContext(c);
    }

    for (modularity::IModuleSetup* m : m_modules) {
        m->onDeinit();
    }

    m_globalModule.onDeinit();

    for (modularity::IModuleSetup* m : m_modules) {
        m->onDestroy();
    }

    m_globalModule.onDestroy();

    // Delete modules
    qDeleteAll(m_modules);
    m_modules.clear();
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

modularity::ContextPtr GuiApp::setupNewContext(const muse::StringList& args)
{
    UNUSED(args);
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
        s->onPreInit(runMode());
    }

    for (modularity::IContextSetup* s : csetups) {
        s->onInit(runMode());
    }

    for (modularity::IContextSetup* s : csetups) {
        s->onAllInited(runMode());
    }

    if (!args.empty()) {
        auto startupScenario = muse::modularity::ioc(ctx)->resolve<IStartupScenario>("app");
        if (startupScenario) {
            std::optional<std::string> sessionType;
            std::optional<project::ProjectFile> scoreFile;
            QString displayNameOverride;

            for (size_t i = 0; i < args.size(); ++i) {
                if (args[i] == "--session-type" && i + 1 < args.size()) {
                    sessionType = args[++i].toStdString();
                } else if (args[i] == "--project-display-name-override" && i + 1 < args.size()) {
                    displayNameOverride = args[++i].toQString();
                } else if (!args[i].startsWith(u"--")) {
                    project::ProjectFile file;
                    file.url = QUrl::fromLocalFile(args[i].toQString());
                    scoreFile = file;
                }
            }

            if (scoreFile.has_value() && !displayNameOverride.isEmpty()) {
                scoreFile.value().displayNameOverride = displayNameOverride;
            }

            startupScenario->setStartupType(sessionType);
            startupScenario->setStartupScoreFile(scoreFile);
        }
    }
#endif

    QQmlApplicationEngine* engine = muse::modularity::ioc(ctx)->resolve<muse::ui::IUiEngine>("app")->qmlAppEngine();

    QObject::connect(engine, &QQmlEngine::warnings, [](const QList<QQmlError>& warnings) {
        for (const QQmlError& e : warnings) {
            LOGE() << "error: " << e.toString().toStdString() << "\n";
        }
    });

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

    muse::ui::QmlApi* windowApi = new muse::ui::QmlApi(qmlCtx, ctx);
    qmlCtx->setContextProperty("api", windowApi);

    QObject* obj = component.create(qmlCtx);
    if (!obj) {
        LOGE() << "Failed to create QML object for new context: " << component.errorString().toStdString() << "\n";
        QCoreApplication::exit(-1);
        return nullptr;
    }

#ifdef MUSE_MULTICONTEXT_WIP
    qmlCtx->setParent(obj);

    {
        QQuickWindow* w = dynamic_cast<QQuickWindow*>(obj);
        QObject::connect(w, &QObject::destroyed, [this, ctx]() {
            destroyContext(ctx);
            if (m_contexts.empty()) {
                QCoreApplication::quit();
            }
        });
    }
#endif

    const auto finalizeStartup = [this, obj, ctx]() {
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
        QQuickWindow* qw = dynamic_cast<QQuickWindow*>(obj);
        qw->setOpacity(0.01);
        qw->setVisible(true);
        auto startupScenario = muse::modularity::ioc(ctx)->resolve<IStartupScenario>("app");
        startupScenario->runAfterSplashScreen();
        haveFinalized = true;
    };

    auto startupScenario = muse::modularity::ioc(ctx)->resolve<IStartupScenario>("app");
    muse::async::Promise<Ret> promise = startupScenario->runOnSplashScreen();
    promise.onResolve(nullptr, [finalizeStartup](Ret) {
        finalizeStartup();
    });

    return ctx;
}

void GuiApp::destroyContext(const modularity::ContextPtr& ctx)
{
    if (!ctx) {
        return;
    }

    LOGI() << "Destroying context with id: " << ctx->id;

    auto it = std::find_if(m_contexts.begin(), m_contexts.end(),
                           [&ctx](const Context& c) { return c.ctx->id == ctx->id; });
    if (it == m_contexts.end()) {
        LOGW() << "Context not found: " << ctx->id;
        return;
    }

    // Engine quit
    muse::modularity::ioc(ctx)->resolve<muse::ui::IUiEngine>("app")->quit();

    for (modularity::IContextSetup* s : it->setups) {
        s->onDeinit();
    }

    qDeleteAll(it->setups);
    m_contexts.erase(it);
    modularity::removeIoC(ctx);
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

#ifndef MUSE_MULTICONTEXT_WIP
    auto startupScenario = modularity::ioc(m_contexts.front().ctx)->resolve<IStartupScenario>("app");

    startupScenario->setStartupType(options.startup.type);

    if (options.startup.projectUrl.has_value()) {
        project::ProjectFile file { options.startup.projectUrl.value() };

        if (options.startup.projectDisplayNameOverride.has_value()) {
            file.displayNameOverride = options.startup.projectDisplayNameOverride.value();
        }

        startupScenario->setStartupScoreFile(file);
    }
#endif

    if (options.app.loggerLevel) {
        m_globalModule.setLoggerLevel(options.app.loggerLevel.value());
    }
}
