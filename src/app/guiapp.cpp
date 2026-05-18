/*
 * Audacity: A Digital Audio Editor
 */
#include "guiapp.h"

#include <QCoreApplication>

#include "framework/global/modularity/ioc.h"
#include "framework/ui/imainwindow.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/actions/actiontypes.h"

#include "appshell/istartupscenario.h"
#include "appshell/internal/splashscreen/splashscreen.h"
#include "project/types/projecttypes.h"

#include "commandlineparser.h"

#include "muse_framework_config.h"

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
    if (m_splashScreen) {
        return;
    }

    m_splashScreen = new appshell::SplashScreen(ctxId, appshell::SplashScreen::Default);
    m_splashScreen->show();
}

void GuiApp::doStartupScenario(const muse::modularity::ContextPtr& ctxId)
{
    auto startupScenario = muse::modularity::ioc(ctxId)->resolve<IStartupScenario>("app");
    IF_ASSERT_FAILED(startupScenario) {
        return;
    }

    const std::shared_ptr<AudacityCmdOptions> options
        = std::dynamic_pointer_cast<AudacityCmdOptions>(contextData(ctxId).options);
    IF_ASSERT_FAILED(options) {
        return;
    }

    std::optional<project::ProjectFile> projectFile;
    if (options->startup.projectUrl.has_value()) {
        project::ProjectFile file;
        file.url = options->startup.projectUrl.value();
        if (options->startup.projectDisplayNameOverride.has_value()) {
            file.displayNameOverride = options->startup.projectDisplayNameOverride.value();
        }
        projectFile = file;
    }

    startupScenario->setStartupType(options->startup.type);
    startupScenario->setStartupProjectFile(projectFile);
    startupScenario->setStartupMediaFiles(options->startup.mediaFiles);
    startupScenario->setRemoveMediaFilesAfterImport(options->startup.removeMediaFilesAfterImport);

    if (options->startup.startupUrl.has_value()) {
        auto dispatcher = muse::modularity::ioc(ctxId)->resolve<muse::actions::IActionsDispatcher>("app");
        if (dispatcher) {
            dispatcher->dispatch("open-url",
                                 muse::actions::ActionData::make_arg1<QString>(options->startup.startupUrl.value()));
        }
    }

    startupScenario->runOnSplashScreen();

    QMetaObject::invokeMethod(qApp, [this, startupScenario]() {
        if (m_splashScreen) {
            m_splashScreen->close();
            delete m_splashScreen;
            m_splashScreen = nullptr;
        }
        startupScenario->runAfterSplashScreen();
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
        onSecondInstanceArgs(args);
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

    // parse arguments forwarded by the second instance
    auto parsed = std::dynamic_pointer_cast<AudacityCmdOptions>(makeContextOptions(muse::StringList(args)));
    if (!parsed) {
        return;
    }

    auto dispatcher = muse::modularity::ioc(ctx)->resolve<muse::actions::IActionsDispatcher>("app");
    if (!dispatcher) {
        return;
    }

    if (parsed->startup.startupUrl.has_value()) {
        dispatcher->dispatch("open-url",
                             muse::actions::ActionData::make_arg1<QString>(parsed->startup.startupUrl.value()));
    }

    if (parsed->startup.projectUrl.has_value()) {
        dispatcher->dispatch("file-open",
                             muse::actions::ActionData::make_arg1<QUrl>(parsed->startup.projectUrl.value()));
    }
}
