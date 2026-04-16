/*
 * Audacity: A Digital Audio Editor
 */
#include "guiapp.h"

#include "modularity/ioc.h"
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
        if (options->startup.cloudProjectId.has_value()) {
            file.cloudProjectId = options->startup.cloudProjectId.value();
        }
        projectFile = file;
    }

    startupScenario->setStartupType(options->startup.type);
    startupScenario->setStartupProjectFile(projectFile);
    startupScenario->setStartupMediaFiles(options->startup.mediaFiles);
    startupScenario->setRemoveMediaFilesAfterImport(options->startup.removeMediaFilesAfterImport);

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
