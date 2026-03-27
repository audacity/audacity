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

#include "appshell/istartupscenario.h"
#include "appshell/internal/splashscreen/splashscreen.h"
#include "project/types/projecttypes.h"

#include "muse_framework_config.h"

#include "log.h"

using namespace muse;
using namespace au::app;
using namespace au::appshell;
namespace project = au::project;

static int m_lastId = 0;

GuiApp::GuiApp(const CommandLineParser::Options& options)
    : BaseApplication(), m_options(options)
{
    setRunMode(IApplication::RunMode::GuiApp);
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
    m_globalModule = new GlobalModule();
    m_globalModule->setApplication(shared_from_this());
    m_globalModule->registerResources();
    m_globalModule->registerExports();
    m_globalModule->registerUiTypes();

    for (modularity::IModuleSetup* m : m_modules) {
        m->setApplication(shared_from_this());
        m->registerResources();
    }

    for (modularity::IModuleSetup* m : m_modules) {
        m->registerExports();
    }

    m_globalModule->resolveImports();
    m_globalModule->registerApi();
    for (modularity::IModuleSetup* m : m_modules) {
        m->registerUiTypes();
        m->resolveImports();
        m->registerApi();
    }

    applyCommandLineOptions(m_options);

    m_globalModule->onPreInit(runMode());
    for (modularity::IModuleSetup* m : m_modules) {
        m->onPreInit(runMode());
    }

    //! FIXME
    //! The launch scenario is contextual, but there is no context here.
    // m_splashScreen = new SplashScreen(iocContext(), SplashScreen::Default);
    // m_splashScreen->show();

    m_globalModule->onInit(runMode());
    for (modularity::IModuleSetup* m : m_modules) {
        m->onInit(runMode());
    }

    m_globalModule->onAllInited(runMode());
    for (modularity::IModuleSetup* m : m_modules) {
        m->onAllInited(runMode());
    }

    // ====================================================
    // Setup modules: onStartApp (on next event loop)
    // ====================================================
    QMetaObject::invokeMethod(qApp, [this]() {
        m_globalModule->onStartApp();
        for (modularity::IModuleSetup* m : m_modules) {
            m->onStartApp();
        }
    }, Qt::QueuedConnection);

    // ====================================================
    // Setup modules: onDelayedInit
    // ====================================================
    m_delayedInitTimer.setSingleShot(true);
    m_delayedInitTimer.setInterval(5000);
    QObject::connect(&m_delayedInitTimer, &QTimer::timeout, [this]() {
        m_globalModule->onDelayedInit();
        for (modularity::IModuleSetup* m : m_modules) {
            m->onDelayedInit();
        }
    });
    m_delayedInitTimer.start();

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
    {
        TRACEFUNC

        m_delayedInitTimer.stop();

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

        // Deinit and delete contexts
        std::vector<modularity::ContextPtr> ctxs = contexts();
        for (auto& c : ctxs) {
            destroyContext(c);
        }

        for (modularity::IModuleSetup* m : m_modules) {
            m->onDeinit();
        }

        m_globalModule->onDeinit();

        for (modularity::IModuleSetup* m : m_modules) {
            m->onDestroy();
        }

        m_globalModule->onDestroy();

        // Delete modules
        qDeleteAll(m_modules);
        m_modules.clear();

        delete m_globalModule;
        m_globalModule = nullptr;

        muse::modularity::resetAll();

        QCoreApplication::processEvents();

        BaseApplication::finish();
    }

    PROFILER_PRINT;
}

GuiApp::Context& GuiApp::context(const muse::modularity::ContextPtr& ctx)
{
    for (Context& c : m_contexts) {
        if (c.ctx->id == ctx->id) {
            return c;
        }
    }

    m_contexts.emplace_back();

    Context& ref = m_contexts.back();
    ref.ctx = ctx;

    modularity::IContextSetup* global = m_globalModule->newContext(ctx);
    if (global) {
        ref.setups.push_back(global);
    }

    for (modularity::IModuleSetup* m : m_modules) {
        modularity::IContextSetup* s = m->newContext(ctx);
        if (s) {
            ref.setups.push_back(s);
        }
    }

    return ref;
}

modularity::ContextPtr GuiApp::setupNewContext(const muse::StringList& args)
{
    UNUSED(args);
#ifndef MUSE_MODULE_MULTIWINDOWS_SINGLEPROC_MODE
    static bool once = false;
    IF_ASSERT_FAILED(!once) {
        return nullptr;
    }
    once = true;
#endif

    modularity::ContextPtr ctxId = std::make_shared<modularity::Context>();
    ++m_lastId;
    ctxId->id = m_lastId;

    LOGI() << "Setting up new context with id: " << ctxId->id << "\n";

    std::vector<muse::modularity::IContextSetup*>& csetups = context(ctxId).setups;

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
        auto startupScenario = muse::modularity::ioc(ctxId)->resolve<IStartupScenario>("app");
        if (startupScenario) {
            std::optional<std::string> sessionType;
            std::optional<project::ProjectFile> projectFile;
            muse::io::paths_t mediaFiles;
            QString displayNameOverride;

            QString cloudProjectId;
            for (size_t i = 0; i < args.size(); ++i) {
                if (args[i] == "--session-type" && i + 1 < args.size()) {
                    sessionType = args[++i].toStdString();
                } else if (args[i] == "--project-display-name-override" && i + 1 < args.size()) {
                    displayNameOverride = args[++i].toQString();
                } else if (args[i] == "--cloud-project-id" && i + 1 < args.size()) {
                    cloudProjectId = args[++i].toQString();
                } else if (!args[i].startsWith(u"--")) {
                    const muse::io::path_t filePath(args[i].toQString());
                    if (project::isAudacityFile(filePath)) {
                        if (!projectFile.has_value()) {
                            project::ProjectFile file;
                            file.url = QUrl::fromLocalFile(args[i].toQString());
                            projectFile = file;
                        }
                        mediaFiles.clear();
                    } else if (!projectFile.has_value()) {
                        mediaFiles.emplace_back(filePath);
                    }
                }
            }

            if (projectFile.has_value()) {
                if (!displayNameOverride.isEmpty()) {
                    projectFile.value().displayNameOverride = displayNameOverride;
                }
                if (!cloudProjectId.isEmpty()) {
                    projectFile.value().cloudProjectId = cloudProjectId;
                }
            }

            startupScenario->setStartupType(sessionType);
            startupScenario->setStartupProjectFile(projectFile);
            startupScenario->setStartupMediaFiles(mediaFiles);
        }
    }

    QQmlApplicationEngine* engine = muse::modularity::ioc(ctxId)->resolve<muse::ui::IUiEngine>("app")->qmlAppEngine();

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
    qmlCtx->setObjectName(QString("QQmlContext: %1").arg(ctxId ? ctxId->id : 0));
    QmlIoCContext* iocCtx = new QmlIoCContext(qmlCtx);
    iocCtx->ctx = ctxId;
    qmlCtx->setContextProperty("ioc_context", QVariant::fromValue(iocCtx));

    muse::ui::QmlApi* windowApi = new muse::ui::QmlApi(qmlCtx, ctxId);
    qmlCtx->setContextProperty("api", windowApi);

    QObject* obj = component.create(qmlCtx);
    if (!obj) {
        LOGE() << "Failed to create QML object for new context: " << component.errorString().toStdString() << "\n";
        QCoreApplication::exit(-1);
        return nullptr;
    }

    // Store the root window for context management
    {
        Context& ctx = context(ctxId);
        ctx.window = dynamic_cast<QQuickWindow*>(obj);
    }

#ifdef MUSE_MODULE_MULTIWINDOWS_SINGLEPROC_MODE
    qmlCtx->setParent(obj);

    {
        QQuickWindow* w = dynamic_cast<QQuickWindow*>(obj);
        QObject::connect(w, &QObject::destroyed, [this, ctxId]() {
            // Null out window pointer — it's already being destroyed
            auto it = std::find_if(m_contexts.begin(), m_contexts.end(),
                                   [&ctxId](const Context& c) { return c.ctx->id == ctxId->id; });
            if (it != m_contexts.end()) {
                it->window = nullptr;
            }
            destroyContext(ctxId);
            if (m_contexts.empty()) {
                QCoreApplication::quit();
            }
        });
    }
#endif

    QMetaObject::invokeMethod(qApp, [this, ctxId, obj]() {
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
        Context& ctx = context(ctxId);
        ctx.window = dynamic_cast<QQuickWindow*>(obj);
        ctx.window->setOpacity(0.01);
        ctx.window->setVisible(true);

        auto startupScenario = muse::modularity::ioc(ctxId)->resolve<IStartupScenario>("app");
        startupScenario->runAfterSplashScreen();
    }, Qt::QueuedConnection);

    return ctxId;
}

void GuiApp::destroyContext(const modularity::ContextPtr& ctx)
{
    TRACEFUNC;

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

    // Stopping the engine and destroying the top level window
    muse::modularity::ioc(ctx)->resolve<muse::ui::IUiEngine>("app")->quit();

    // The window could be already destroyed at this point
    if (QQuickWindow* win = it->window) {
        it->window = nullptr;
        delete win;
    }

    // QML objects are not destroyed immediately on engine->quit().
    // Some models or controllers, could use injects in their destructors,
    // so we have to defer context destruction to the next event loop iteration
    QTimer::singleShot(0, [this, ctx]{
        // Flush any deleteLater() calls triggered during window destruction.
        QCoreApplication::sendPostedEvents(nullptr, QEvent::DeferredDelete);

        auto it = std::find_if(m_contexts.begin(), m_contexts.end(),
                               [&ctx](const Context& c) { return c.ctx->id == ctx->id; });
        if (it == m_contexts.end()) {
            return;
        }

        // Now we can safely destroy the context
        for (modularity::IContextSetup* s : it->setups) {
            s->onDeinit();
        }

        qDeleteAll(it->setups);
        m_contexts.erase(it);
    });

    // Processing QML garbage collection, and context deletion
    QCoreApplication::processEvents();
}

size_t GuiApp::contextCount() const
{
    return m_contexts.size();
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
    if (options.app.factoryReset) {
        appshellConfiguration()->revertToFactorySettings();
    }

    if (options.app.loggerLevel) {
        m_globalModule->setLoggerLevel(options.app.loggerLevel.value());
    }
}
