/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include "app.h"

#include <QApplication>
#include <QQmlApplicationEngine>
#include <QQuickWindow>
#include <QStyleHints>
#ifndef Q_OS_WASM
#include <QThreadPool>
#endif

#include "appshell/view/internal/splashscreen/splashscreen.h"
#include "appshell/view/dockwindow/docksetup.h"

#include "modularity/ioc.h"
#include "ui/internal/uiengine.h"
#include "muversion.h"

#include "framework/global/globalmodule.h"

#include "log.h"

using namespace mu::app;
using namespace mu::appshell;

//! NOTE Separately to initialize logger and profiler as early as possible
static mu::GlobalModule globalModule;

App::App()
{
}

void App::addModule(modularity::IModuleSetup* module)
{
    m_modules.push_back(module);
}

int App::run(int argc, char** argv)
{
    // ====================================================
    // Setup global Qt application variables
    // ====================================================
    qputenv("QT_STYLE_OVERRIDE", "Fusion");
    qputenv("QML_DISABLE_DISK_CACHE", "true");

#ifdef Q_OS_LINUX
    if (qEnvironmentVariable("QT_QPA_PLATFORM") != "offscreen") {
        qputenv("QT_QPA_PLATFORMTHEME", "gtk3");
    }
#endif

    const char* appName;
    if (MUVersion::unstable()) {
        appName  = "MuseScore4Development";
    } else {
        appName  = "MuseScore4";
    }

#ifdef Q_OS_WIN
    // NOTE: There are some problems with rendering the application window on some integrated graphics processors
    //       see https://github.com/musescore/MuseScore/issues/8270
    QCoreApplication::setAttribute(Qt::AA_UseOpenGLES);

    if (!qEnvironmentVariableIsSet("QT_OPENGL_BUGLIST")) {
        qputenv("QT_OPENGL_BUGLIST", ":/resources/win_opengl_buglist.json");
    }
#endif

#ifdef MU_QT5_COMPAT
    QCoreApplication::setAttribute(Qt::AA_EnableHighDpiScaling);
#endif

    //! NOTE: For unknown reasons, Linux scaling for 1 is defined as 1.003 in fractional scaling.
    //!       Because of this, some elements are drawn with a shift on the score.
    //!       Let's make a Linux hack and round values above 0.75(see RoundPreferFloor)
#ifdef Q_OS_LINUX
    QGuiApplication::setHighDpiScaleFactorRoundingPolicy(Qt::HighDpiScaleFactorRoundingPolicy::RoundPreferFloor);
#elif defined(Q_OS_WIN)
    QGuiApplication::setHighDpiScaleFactorRoundingPolicy(Qt::HighDpiScaleFactorRoundingPolicy::PassThrough);
#endif

    QGuiApplication::styleHints()->setMousePressAndHoldInterval(250);

#ifndef MU_QT5_COMPAT
    // Necessary for QQuickWidget, but potentially suboptimal for performance.
    // Remove as soon as possible.
    QQuickWindow::setGraphicsApi(QSGRendererInterface::OpenGL);
#endif

    // ====================================================
    // Parse command line options
    // ====================================================
    CommandLineParser commandLineParser;
    commandLineParser.init();
    commandLineParser.parse(argc, argv);

    IApplication::RunMode runMode = commandLineParser.runMode();
    QCoreApplication* app = nullptr;

    if (runMode == IApplication::RunMode::AudioPluginRegistration) {
        app = new QCoreApplication(argc, argv);
    } else {
        app = new QApplication(argc, argv);
    }

    QCoreApplication::setApplicationName(appName);
    QCoreApplication::setOrganizationName("MuseScore");
    QCoreApplication::setOrganizationDomain("musescore.org");
    QCoreApplication::setApplicationVersion(QString::fromStdString(MUVersion::fullVersion().toStdString()));

#if !defined(Q_OS_WIN) && !defined(Q_OS_DARWIN) && !defined(Q_OS_WASM)
    // Any OS that uses Freedesktop.org Desktop Entry Specification (e.g. Linux, BSD)
    QGuiApplication::setDesktopFileName("org.musescore.MuseScore" MUSESCORE_INSTALL_SUFFIX ".desktop");
#endif

    commandLineParser.processBuiltinArgs(*app);

    // ====================================================
    // Setup modules: Resources, Exports, Imports, UiTypes
    // ====================================================
    globalModule.registerResources();
    globalModule.registerExports();
    globalModule.registerUiTypes();

    for (mu::modularity::IModuleSetup* m : m_modules) {
        m->registerResources();
    }

    for (mu::modularity::IModuleSetup* m : m_modules) {
        m->registerExports();
    }

    globalModule.resolveImports();
    globalModule.registerApi();
    for (mu::modularity::IModuleSetup* m : m_modules) {
        m->registerUiTypes();
        m->resolveImports();
        m->registerApi();
    }

    // ====================================================
    // Setup modules: apply the command line options
    // ====================================================
    muapplication()->setRunMode(runMode);
    applyCommandLineOptions(commandLineParser.options(), runMode);

    // ====================================================
    // Setup modules: onPreInit
    // ====================================================
    globalModule.onPreInit(runMode);
    for (mu::modularity::IModuleSetup* m : m_modules) {
        m->onPreInit(runMode);
    }

#ifdef MUE_BUILD_APPSHELL_MODULE
    SplashScreen* splashScreen = nullptr;
    if (runMode == IApplication::RunMode::GuiApp) {
        if (multiInstancesProvider()->isMainInstance()) {
            splashScreen = new SplashScreen(SplashScreen::Default);
        } else {
            const project::ProjectFile& file = startupScenario()->startupScoreFile();
            if (file.isValid()) {
                if (file.hasDisplayName()) {
                    splashScreen = new SplashScreen(SplashScreen::ForNewInstance, false, file.displayName(true /* includingExtension */));
                } else {
                    splashScreen = new SplashScreen(SplashScreen::ForNewInstance, false);
                }
            } else if (startupScenario()->isStartWithNewFileAsSecondaryInstance()) {
                splashScreen = new SplashScreen(SplashScreen::ForNewInstance, true);
            } else {
                splashScreen = new SplashScreen(SplashScreen::Default);
            }
        }
    }

    if (splashScreen) {
        splashScreen->show();
    }
#endif

    // ====================================================
    // Setup modules: onInit
    // ====================================================
    globalModule.onInit(runMode);
    for (mu::modularity::IModuleSetup* m : m_modules) {
        m->onInit(runMode);
    }

    // ====================================================
    // Setup modules: onAllInited
    // ====================================================
    globalModule.onAllInited(runMode);
    for (mu::modularity::IModuleSetup* m : m_modules) {
        m->onAllInited(runMode);
    }

    // ====================================================
    // Setup modules: onStartApp (on next event loop)
    // ====================================================
    QMetaObject::invokeMethod(qApp, [this]() {
        globalModule.onStartApp();
        for (mu::modularity::IModuleSetup* m : m_modules) {
            m->onStartApp();
        }
    }, Qt::QueuedConnection);

    // ====================================================
    // Run
    // ====================================================

    switch (runMode) {
    case IApplication::RunMode::ConsoleApp: {
        // ====================================================
        // Process Autobot
        // ====================================================
        CommandLineParser::Autobot autobot = commandLineParser.autobot();
        if (!autobot.testCaseNameOrFile.isEmpty()) {
            QMetaObject::invokeMethod(qApp, [this, autobot]() {
                    processAutobot(autobot);
                }, Qt::QueuedConnection);
        } else {
            // ====================================================
            // Process Diagnostic
            // ====================================================
            CommandLineParser::Diagnostic diagnostic = commandLineParser.diagnostic();
            if (diagnostic.type != CommandLineParser::DiagnosticType::Undefined) {
                QMetaObject::invokeMethod(qApp, [this, diagnostic]() {
                        int code = processDiagnostic(diagnostic);
                        qApp->exit(code);
                    }, Qt::QueuedConnection);
            } else {
                // ====================================================
                // Process Converter
                // ====================================================
                CommandLineParser::ConverterTask task = commandLineParser.converterTask();
                QMetaObject::invokeMethod(qApp, [this, task]() {
                        int code = processConverter(task);
                        qApp->exit(code);
                    }, Qt::QueuedConnection);
            }
        }
    } break;
    case IApplication::RunMode::GuiApp: {
#ifdef MUE_BUILD_APPSHELL_MODULE
        // ====================================================
        // Setup Qml Engine
        // ====================================================
        QQmlApplicationEngine* engine = new QQmlApplicationEngine();

        dock::DockSetup::setup(engine);

#if defined(Q_OS_WIN)
        const QString mainQmlFile = "/platform/win/Main.qml";
#elif defined(Q_OS_MACOS)
        const QString mainQmlFile = "/platform/mac/Main.qml";
#elif defined(Q_OS_LINUX) || defined(Q_OS_FREEBSD)
        const QString mainQmlFile = "/platform/linux/Main.qml";
#elif defined(Q_OS_WASM)
        const QString mainQmlFile = "/Main.wasm.qml";
#endif
        //! NOTE Move ownership to UiEngine
        ui::UiEngine::instance()->moveQQmlEngine(engine);

#ifdef MUE_ENABLE_LOAD_QML_FROM_SOURCE
        const QUrl url(QString(appshell_QML_IMPORT) + mainQmlFile);
#else
        const QUrl url(QStringLiteral("qrc:/qml") + mainQmlFile);
#endif

        QObject::connect(engine, &QQmlApplicationEngine::objectCreated,
                         app, [this, url, splashScreen](QObject* obj, const QUrl& objUrl) {
                if (!obj && url == objUrl) {
                    LOGE() << "failed Qml load\n";
                    QCoreApplication::exit(-1);
                    return;
                }

                if (url == objUrl) {
                    // ====================================================
                    // Setup modules: onDelayedInit
                    // ====================================================

                    globalModule.onDelayedInit();
                    for (mu::modularity::IModuleSetup* m : m_modules) {
                        m->onDelayedInit();
                    }

                    if (splashScreen) {
                        splashScreen->close();
                        delete splashScreen;
                    }

                    startupScenario()->run();
                }
            }, Qt::QueuedConnection);

        QObject::connect(engine, &QQmlEngine::warnings, [](const QList<QQmlError>& warnings) {
                for (const QQmlError& e : warnings) {
                    LOGE() << "error: " << e.toString().toStdString() << "\n";
                }
            });

        // ====================================================
        // Load Main qml
        // ====================================================

        //! Needs to be set because we use transparent windows for PopupView.
        //! Needs to be called before any QQuickWindows are shown.
        QQuickWindow::setDefaultAlphaBuffer(true);

        engine->load(url);
#endif // MUE_BUILD_APPSHELL_MODULE
    } break;
    case IApplication::RunMode::AudioPluginRegistration: {
        CommandLineParser::AudioPluginRegistration pluginRegistration = commandLineParser.audioPluginRegistration();

        QMetaObject::invokeMethod(qApp, [this, pluginRegistration]() {
                int code = processAudioPluginRegistration(pluginRegistration);
                qApp->exit(code);
            }, Qt::QueuedConnection);
    } break;
    }

    // ====================================================
    // Run main loop
    // ====================================================
    int retCode = app->exec();

    // ====================================================
    // Quit
    // ====================================================

    PROFILER_PRINT;

    // Wait Thread Poll
#ifndef Q_OS_WASM
    QThreadPool* globalThreadPool = QThreadPool::globalInstance();
    if (globalThreadPool) {
        LOGI() << "activeThreadCount: " << globalThreadPool->activeThreadCount();
        globalThreadPool->waitForDone();
    }
#endif

#ifdef MUE_BUILD_APPSHELL_MODULE
    // Engine quit
    ui::UiEngine::instance()->quit();
#endif

    // Deinit

    globalModule.invokeQueuedCalls();

    for (mu::modularity::IModuleSetup* m : m_modules) {
        m->onDeinit();
    }

    globalModule.onDeinit();

    for (mu::modularity::IModuleSetup* m : m_modules) {
        m->onDestroy();
    }

    globalModule.onDestroy();

    // Delete modules
    qDeleteAll(m_modules);
    m_modules.clear();
    mu::modularity::ioc()->reset();

    delete app;

    return retCode;
}

void App::applyCommandLineOptions(const CommandLineParser::Options& options, IApplication::RunMode runMode)
{
    uiConfiguration()->setPhysicalDotsPerInch(options.ui.physicalDotsPerInch);

    notationConfiguration()->setTemplateModeEnabled(options.notation.templateModeEnabled);
    notationConfiguration()->setTestModeEnabled(options.notation.testModeEnabled);

    if (runMode == IApplication::RunMode::ConsoleApp) {
        project::MigrationOptions migration;
        migration.appVersion = mu::engraving::Constants::MSC_VERSION;

        //! NOTE Don't ask about migration in convert mode
        migration.isAskAgain = false;

        if (options.project.fullMigration) {
            bool isMigration = options.project.fullMigration.value();
            migration.isApplyMigration = isMigration;
            migration.isApplyEdwin = isMigration;
            migration.isApplyLeland = isMigration;
        }

        //! NOTE Don't write to settings, just on current session
        for (project::MigrationType type : project::allMigrationTypes()) {
            projectConfiguration()->setMigrationOptions(type, migration, false);
        }
    }

#ifdef MUE_BUILD_IMAGESEXPORT_MODULE
    imagesExportConfiguration()->setTrimMarginPixelSize(options.exportImage.trimMarginPixelSize);
    imagesExportConfiguration()->setExportPngDpiResolutionOverride(options.exportImage.pngDpiResolution);
#endif

#ifdef MUE_BUILD_VIDEOEXPORT_MODULE
    videoExportConfiguration()->setResolution(options.exportVideo.resolution);
    videoExportConfiguration()->setFps(options.exportVideo.fps);
    videoExportConfiguration()->setLeadingSec(options.exportVideo.leadingSec);
    videoExportConfiguration()->setTrailingSec(options.exportVideo.trailingSec);
#endif

#ifdef MUE_BUILD_IMPORTEXPORT_MODULE
    audioExportConfiguration()->setExportMp3BitrateOverride(options.exportAudio.mp3Bitrate);
    midiImportExportConfiguration()->setMidiImportOperationsFile(options.importMidi.operationsFile);
    guitarProConfiguration()->setLinkedTabStaffCreated(options.guitarPro.linkedTabStaffCreated);
    guitarProConfiguration()->setExperimental(options.guitarPro.experimental);
#endif

    if (options.app.revertToFactorySettings) {
        appshellConfiguration()->revertToFactorySettings(options.app.revertToFactorySettings.value());
    }

    if (runMode == IApplication::RunMode::GuiApp) {
        startupScenario()->setStartupType(options.startup.type);

        if (options.startup.scoreUrl.has_value()) {
            project::ProjectFile file { options.startup.scoreUrl.value() };

            if (options.startup.scoreDisplayNameOverride.has_value()) {
                file.displayNameOverride = options.startup.scoreDisplayNameOverride.value();
            }

            startupScenario()->setStartupScoreFile(file);
        }
    }

    if (options.app.loggerLevel) {
        globalModule.setLoggerLevel(options.app.loggerLevel.value());
    }
}

int App::processConverter(const CommandLineParser::ConverterTask& task)
{
    Ret ret = make_ret(Ret::Code::Ok);
    io::path_t stylePath = task.params[CommandLineParser::ParamKey::StylePath].toString();
    bool forceMode = task.params[CommandLineParser::ParamKey::ForceMode].toBool();
    String soundProfile = task.params[CommandLineParser::ParamKey::SoundProfile].toString();

    if (!soundProfile.isEmpty() && !soundProfilesRepository()->containsProfile(soundProfile)) {
        LOGE() << "Unknown sound profile: " << soundProfile;
        soundProfile.clear();
    }

    switch (task.type) {
    case CommandLineParser::ConvertType::Batch:
        ret = converter()->batchConvert(task.inputFile, stylePath, forceMode, soundProfile);
        break;
    case CommandLineParser::ConvertType::File:
        ret = converter()->fileConvert(task.inputFile, task.outputFile, stylePath, forceMode, soundProfile);
        break;
    case CommandLineParser::ConvertType::ConvertScoreParts:
        ret = converter()->convertScoreParts(task.inputFile, task.outputFile, stylePath);
        break;
    case CommandLineParser::ConvertType::ExportScoreMedia: {
        io::path_t highlightConfigPath = task.params[CommandLineParser::ParamKey::HighlightConfigPath].toString();
        ret = converter()->exportScoreMedia(task.inputFile, task.outputFile, highlightConfigPath, stylePath, forceMode);
    } break;
    case CommandLineParser::ConvertType::ExportScoreMeta:
        ret = converter()->exportScoreMeta(task.inputFile, task.outputFile, stylePath, forceMode);
        break;
    case CommandLineParser::ConvertType::ExportScoreParts:
        ret = converter()->exportScoreParts(task.inputFile, task.outputFile, stylePath, forceMode);
        break;
    case CommandLineParser::ConvertType::ExportScorePartsPdf:
        ret = converter()->exportScorePartsPdfs(task.inputFile, task.outputFile, stylePath, forceMode);
        break;
    case CommandLineParser::ConvertType::ExportScoreTranspose: {
        std::string scoreTranspose = task.params[CommandLineParser::ParamKey::ScoreTransposeOptions].toString().toStdString();
        ret = converter()->exportScoreTranspose(task.inputFile, task.outputFile, scoreTranspose, stylePath, forceMode);
    } break;
    case CommandLineParser::ConvertType::ExportScoreVideo: {
        ret = converter()->exportScoreVideo(task.inputFile, task.outputFile);
    } break;
    case CommandLineParser::ConvertType::SourceUpdate: {
        std::string scoreSource = task.params[CommandLineParser::ParamKey::ScoreSource].toString().toStdString();
        ret = converter()->updateSource(task.inputFile, scoreSource, forceMode);
    } break;
    }

    if (!ret) {
        LOGE() << "failed convert, error: " << ret.toString();
    }

    return ret.code();
}

int App::processDiagnostic(const CommandLineParser::Diagnostic& task)
{
    if (!diagnosticDrawProvider()) {
        return make_ret(Ret::Code::NotSupported);
    }

    Ret ret = make_ret(Ret::Code::Ok);

    if (task.input.isEmpty()) {
        return make_ret(Ret::Code::UnknownError);
    }

    io::paths_t input;
    for (const QString& p : task.input) {
        input.push_back(p);
    }

    io::path_t output = task.output;

    if (output.empty()) {
        output = "./";
    }

    switch (task.type) {
    case CommandLineParser::DiagnosticType::GenDrawData:
        ret = diagnosticDrawProvider()->generateDrawData(input.front(), output);
        break;
    case CommandLineParser::DiagnosticType::ComDrawData:
        IF_ASSERT_FAILED(input.size() == 2) {
            return make_ret(Ret::Code::UnknownError);
        }
        ret = diagnosticDrawProvider()->compareDrawData(input.at(0), input.at(1), output);
        break;
    case CommandLineParser::DiagnosticType::DrawDataToPng:
        ret = diagnosticDrawProvider()->drawDataToPng(input.front(), output);
        break;
    case CommandLineParser::DiagnosticType::DrawDiffToPng: {
        io::path_t diffPath = input.at(0);
        io::path_t refPath;
        if (input.size() > 1) {
            refPath = input.at(1);
        }
        ret = diagnosticDrawProvider()->drawDiffToPng(diffPath, refPath, output);
    } break;
    default:
        break;
    }

    if (!ret) {
        LOGE() << "diagnostic ret: " << ret.toString();
    }

    return ret.code();
}

int App::processAudioPluginRegistration(const CommandLineParser::AudioPluginRegistration& task)
{
    Ret ret = make_ret(Ret::Code::Ok);

    if (task.failedPlugin) {
        ret = registerAudioPluginsScenario()->registerFailedPlugin(task.pluginPath, task.failCode);
    } else {
        ret = registerAudioPluginsScenario()->registerPlugin(task.pluginPath);
    }

    if (!ret) {
        LOGE() << ret.toString();
    }

    return ret.code();
}

void App::processAutobot(const CommandLineParser::Autobot& task)
{
    using namespace mu::autobot;
    async::Channel<StepInfo, Ret> stepCh = autobot()->stepStatusChanged();
    stepCh.onReceive(nullptr, [](const StepInfo& step, const Ret& ret){
        if (!ret) {
            LOGE() << "failed step: " << step.name << ", ret: " << ret.toString();
            qApp->exit(ret.code());
        } else {
            LOGI() << "success step: " << step.name << ", ret: " << ret.toString();
        }
    });

    async::Channel<io::path_t, IAutobot::Status> statusCh = autobot()->statusChanged();
    statusCh.onReceive(nullptr, [](const io::path_t& path, IAutobot::Status st){
        if (st == IAutobot::Status::Finished) {
            LOGI() << "success finished, path: " << path;
            qApp->exit(0);
        }
    });

    IAutobot::Options opt;
    opt.context = task.testCaseContextNameOrFile;
    opt.contextVal = task.testCaseContextValue.toStdString();
    opt.func = task.testCaseFunc.toStdString();
    opt.funcArgs = task.testCaseFuncArgs.toStdString();

    autobot()->execScript(task.testCaseNameOrFile, opt);
}
