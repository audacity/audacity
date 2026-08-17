/*
 * Audacity: A Digital Audio Editor
 */
#include "guiapp.h"

#include <QCoreApplication>
#include <optional>

#include "framework/global/async/async.h"
#include "framework/global/modularity/ioc.h"
#include "framework/ui/imainwindow.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/actions/actiontypes.h"

#include "appshell/istartupscenario.h"
#include "appshell/internal/splashscreen/splashscreen.h"
#include "project/types/projecttypes.h"

#include "commandlineparser.h"

#ifdef MUSE_MODULE_TESTFLOW
#include <cstdlib>
#include <iostream>

#include <QDir>
#include <QFileInfo>
#include <QTimer>

#include "testflow/itestflow.h"
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

    std::optional<std::string> startupType = options->startup.type;
#ifdef MUSE_MODULE_TESTFLOW
    if (!options->testflow.testCaseNameOrFile.isEmpty()) {
        startupType = "testflow-headless";
    }
#endif
    startupScenario->setStartupType(startupType);

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

        runTestflowIfRequired(ctxId, options);
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

#ifdef MUSE_MODULE_TESTFLOW
    if (!options->testflow.testCaseNameOrFile.isEmpty()) {
        // Avoid blocking dialogs.
        // TODO: This is a temporary workaround. The proper implementation seems to be
        // 1. Reorganize start-up so that `TestflowInteractive`'s lifespan encompasses
        // all dialogs (plugin-scan, save-project-on-close, etc),
        // 2. Override `TestflowInteractive` methods with something other than just
        // forwarding to the real IInteractive implementation.
        appshellConfiguration()->setHasCompletedFirstLaunchSetup(true);
        appshellConfiguration()->setWelcomeDialogShowOnStartup(false);
        qputenv("AU_SKIP_PLUGIN_VALIDATION_PROMPT", "1");
        qputenv("AU_SKIP_SAVE_PROJECT_PROMPT", "1");
    }
#endif
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

    if (!parsed->startup.mediaFiles.empty()) {
        QStringList files;
        files.reserve(static_cast<int>(parsed->startup.mediaFiles.size()));
        for (const auto& file : parsed->startup.mediaFiles) {
            files << file.toQString();
        }
        dispatcher->dispatch("project-import-startup-media",
                             muse::actions::ActionData::make_arg2<QStringList, bool>(
                                 files, parsed->startup.removeMediaFilesAfterImport));
    }
}

namespace {
//! NOTE Ends the process immediately, skipping the regular teardown: actions
//! queued by the last test steps would be delivered mid-teardown, where
//! late-rejected Interactive::openSync promises write to dead stack frames
//! and crash (muse framework issue). Callers must have flushed any output
//! they care about (std::_Exit flushes nothing).
[[noreturn]] void exitWithoutTeardown(int code)
{
    std::_Exit(code);
}

muse::io::path_t resolveScriptPath(const QString& nameOrFile, const muse::io::paths_t& scriptsDirs)
{
    if (QFileInfo::exists(nameOrFile)) {
        return muse::io::path_t(QFileInfo(nameOrFile).absoluteFilePath());
    }

    for (const muse::io::path_t& dir : scriptsDirs) {
        for (const QString& candidate : { nameOrFile, nameOrFile + ".js" }) {
            muse::io::path_t path = muse::io::path_t(dir.toQString() + "/" + candidate);
            if (QFileInfo::exists(path.toQString())) {
                return path;
            }
        }
    }

    return {};
}
}

void GuiApp::runTestflowIfRequired(const muse::modularity::ContextPtr& ctxId, const std::shared_ptr<AudacityCmdOptions>& options)
{
#ifdef MUSE_MODULE_TESTFLOW
    const QString nameOrFile = options->testflow.testCaseNameOrFile;
    if (nameOrFile.isEmpty()) {
        return;
    }

    const muse::io::path_t scriptPath = resolveScriptPath(nameOrFile, testflowConfiguration()->scriptsDirPaths());

    if (scriptPath.empty()) {
        std::cout << "[testflow] FAILED: script not found: " << nameOrFile.toStdString() << std::endl;
        exitWithoutTeardown(2);
    }

    std::cout << "[testflow] running script=" << QDir::cleanPath(scriptPath.toQString()).toStdString() << std::endl;

    //! NOTE There is no reliable "QML fully loaded" signal
    //! (IStartupScenario::startupCompleted is set before the startup page opens),
    //! so give the UI time to settle before the first step fires.
    // TODO: can `StartupScenario::startupCompleted()` return a notification and be used here?
    // (Bearing in mind that `StartupScenario` isn't global)
    const int settleMs = qEnvironmentVariableIsSet("AU_TESTFLOW_STARTUP_DELAY_MS")
                         ? qEnvironmentVariableIntValue("AU_TESTFLOW_STARTUP_DELAY_MS")
                         : 3000;

    QTimer::singleShot(settleMs, qApp, [this, ctxId, options, scriptPath]() {
        auto testflow = muse::modularity::ioc(ctxId)->resolve<muse::testflow::ITestflow>("app");
        IF_ASSERT_FAILED(testflow) {
            exitWithoutTeardown(2);
        }

        if (!options->testflow.testCaseSpeed.isEmpty()) {
            testflow->setSpeedMode(muse::testflow::speedModeFromString(options->testflow.testCaseSpeed));
        }

        muse::testflow::ITestflow::Options opt;
        opt.context = muse::io::path_t(options->testflow.testCaseContextNameOrFile);
        opt.contextVal = options->testflow.testCaseContextValue.toStdString();
        opt.func = options->testflow.testCaseFunc.toStdString();
        opt.funcArgs = options->testflow.testCaseFuncArgs.toStdString();

        //! NOTE A script that fails to load (syntax error, no main()) or never
        //! runs a test case still ends with status Finished. Workaround: count the steps executed.
        // TODO handle this in `Testflow` impl
        auto startedSteps = std::make_shared<int>(0);
        testflow->stepStatusChanged().onReceive(this, [startedSteps](const muse::testflow::StepInfo& step, const muse::Ret&) {
            if (step.status == muse::testflow::StepStatus::Started) {
                ++(*startedSteps);
            }
        });

        testflow->execScript(scriptPath, opt);

        const muse::testflow::ITestflow::Status status = testflow->status();
        const bool ok = status == muse::testflow::ITestflow::Status::Finished && *startedSteps > 0;

        std::cout << "[testflow] " << (ok ? "PASSED" : "FAILED")
                  << " status=" << muse::testflow::ITestflow::statusToString(status).toStdString()
                  << " steps=" << *startedSteps
                  << " script=" << scriptPath.toStdString()
                  << " reports=" << testflowConfiguration()->reportsPath().toStdString() << std::endl;
        if (*startedSteps == 0) {
            std::cout << "[testflow] no steps were executed"
                      << " - does the script define main() and run a test case?" << std::endl;
        }

        //! NOTE The verdict is already reported and the report file flushed,
        //! so the process can end here.
        exitWithoutTeardown(ok ? 0 : 1);
    });
#else
    UNUSED(ctxId);
    UNUSED(options);
#endif
}
