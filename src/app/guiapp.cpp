/*
 * Audacity: A Digital Audio Editor
 */
#include "guiapp.h"

#include <QCoreApplication>
#include <optional>

#include "framework/global/async/async.h"
#include "framework/global/modularity/ioc.h"
#include "framework/ui/imainwindow.h"
#include "framework/rcommand/icommanddispatcher.h"

#include "appshell/istartupscenario.h"
#include "appshell/internal/splashscreen/splashscreen.h"
#include "au3cloud/cloudcommands.h"
#include "project/projectcommands.h"
#include "project/types/projecttypes.h"

#include "commandlineparser.h"

#ifdef MUSE_MODULE_TESTFLOW
#include "testflowrunner.h"
#endif

#include "log.h"

using namespace muse;
using namespace au::app;
using namespace au::appshell;
namespace project = au::project;

GuiApp::GuiApp(const std::shared_ptr<AudacityCmdOptions>& options)
    : muse::ui::GuiApplication(options)
{
}

std::shared_ptr<muse::CmdOptions> GuiApp::makeContextOptions(const muse::StringList& args) const
{
    if (args.empty()) {
        return m_appOptions;
    }

    std::vector<std::string> args_ = args.toStdStringList();
    args_.insert(args_.begin(), "dummy/path/to/app.exe"); // argv[0] placeholder
    const int argc = static_cast<int>(args_.size());
    std::vector<char*> argv(argc + 1, nullptr);
    for (int i = 0; i < argc; ++i) {
        argv[i] = args_[i].data();
    }

    CommandLineParser parser;
    parser.init();
    parser.parse(argc, argv.data());
    return parser.options();
}

QString GuiApp::mainWindowQmlPath(const QString& platform) const
{
    return QString(":/qt/qml/Audacity/AppShell/platform/%1/Main.qml").arg(platform);
}

void GuiApp::showContextSplash(const muse::modularity::ContextPtr& ctxId)
{
    if (!multiwindowsProvider()->isFirstWindow()) {
        return;
    }

    m_splashScreen = new appshell::SplashScreen(ctxId, appshell::SplashScreen::Default);
    m_splashScreen->show();
}

void GuiApp::doStartupScenario(const muse::modularity::ContextPtr& ctxId)
{
    const std::shared_ptr<AudacityCmdOptions> options
        = std::dynamic_pointer_cast<AudacityCmdOptions>(contextData(ctxId).options);
    IF_ASSERT_FAILED(options) {
        return;
    }

#ifdef MUSE_MODULE_TESTFLOW
    TestflowRunner::prepare(ctxId, options->testflow);
#endif

    auto startupScenario = muse::modularity::ioc(ctxId)->resolve<IStartupScenario>("app");
    IF_ASSERT_FAILED(startupScenario) {
        return;
    }

    startupScenario->setStartupType(options->startup.type);
    if (options->startup.projectUrl.has_value()) {
        project::ProjectFile file;
        file.url = options->startup.projectUrl.value();
        if (options->startup.projectDisplayNameOverride.has_value()) {
            file.displayNameOverride = options->startup.projectDisplayNameOverride.value();
        }
        startupScenario->setStartupProjectFile(file);
    }

    startupScenario->setStartupMediaFiles(options->startup.mediaFiles);
    startupScenario->setRemoveMediaFilesAfterImport(options->startup.removeMediaFilesAfterImport);
    if (options->startup.startupUrl.has_value()) {
        startupScenario->setStartupUrl(options->startup.startupUrl.value());
    }

    startupScenario->runOnSplashScreen();

    QMetaObject::invokeMethod(qApp, [this, ctxId, startupScenario, options]() {
        if (m_splashScreen) {
            m_splashScreen->close();
            delete m_splashScreen;
            m_splashScreen = nullptr;
        }
        startupScenario->runAfterSplashScreen();

#ifdef MUSE_MODULE_TESTFLOW
        TestflowRunner::runIfRequested(ctxId, options->testflow);
#endif
    }, Qt::QueuedConnection);
}

void GuiApp::applyCommandLineOptions(const std::shared_ptr<muse::CmdOptions>& opt)
{
    BaseApplication::applyCommandLineOptions(opt);

    std::shared_ptr<AudacityCmdOptions> options = std::dynamic_pointer_cast<AudacityCmdOptions>(opt);
    IF_ASSERT_FAILED(options) {
        return;
    }

    if (options->app.revertToFactorySettings) {
        appshellConfiguration()->revertToFactorySettings();
    }
}

void GuiApp::doSetup(const std::shared_ptr<muse::CmdOptions>& options)
{
    muse::ui::GuiApplication::doSetup(options);

    if (qEnvironmentVariableIsSet("AU_ALLOW_MULTIPLE_PROCESSES")) {
        return;
    }

    const QString appId = QCoreApplication::applicationName();
    if (!m_singleInstance.start(appId)) {
        return;
    }

    m_singleInstance.messageReceived().onReceive(this, [this](const QStringList& args) {
        muse::async::Async::call(this, [this, args]() {
            onSecondInstanceArgs(args);
        });
    });
}

void GuiApp::onSecondInstanceArgs(const QStringList& args)
{
    LOGI() << "second instance handed off args: " << args;

    // Raise the first window when the instance is activated
    // TODO: define rules which window should be activated, ie first, last, last used
    const auto& contexts = muse::ui::GuiApplication::contexts();
    IF_ASSERT_FAILED(!contexts.empty()) {
        return;
    }
    const auto& ctx = contexts.front();

    auto window = muse::modularity::ioc(ctx)->resolve<muse::ui::IMainWindow>("app");
    if (window) {
        window->requestShowOnFront();
    }

    auto parsed = std::dynamic_pointer_cast<AudacityCmdOptions>(makeContextOptions(muse::StringList(args)));
    if (!parsed) {
        return;
    }

    auto dispatcher = muse::modularity::ioc(ctx)->resolve<muse::rcommand::ICommandDispatcher>("app");
    if (!dispatcher) {
        return;
    }

    if (parsed->startup.startupUrl.has_value()) {
        dispatcher->dispatch(au::au3cloud::CLOUD_OPEN_URL_COMMAND, {
            { au::au3cloud::CLOUD_OPEN_URL_URL_PARAM, muse::Val(parsed->startup.startupUrl.value().toStdString()) }
        });
    }

    if (parsed->startup.projectUrl.has_value()) {
        dispatcher->dispatch(project::PROJECT_OPEN_COMMAND, {
            { project::PROJECT_URL_PARAM, muse::Val(parsed->startup.projectUrl.value().toString().toStdString()) }
        });
    }

    if (!parsed->startup.mediaFiles.empty()) {
        muse::ValList files;
        files.reserve(parsed->startup.mediaFiles.size());
        for (const auto& file : parsed->startup.mediaFiles) {
            files.emplace_back(file.toStdString());
        }
        dispatcher->dispatch(project::PROJECT_IMPORT_STARTUP_MEDIA_COMMAND, {
            { project::PROJECT_FILES_PARAM, muse::Val(files) },
            { project::PROJECT_REMOVE_AFTER_IMPORT_PARAM, muse::Val(parsed->startup.removeMediaFilesAfterImport) }
        });
    }
}
