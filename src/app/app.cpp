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

#include "modularity/ioc.h"
#include "ui/internal/uiengine.h"

#include "framework/global/globalmodule.h"
#include "framework/global/internal/baseapplication.h"

#include "log.h"

#define AU_BUILD_APPSHELL_MODULE

using namespace muse;
using namespace au::app;
using namespace au::appshell;

//! NOTE Separately to initialize logger and profiler as early as possible
static GlobalModule globalModule;

static void app_init_qrc()
{
    Q_INIT_RESOURCE(app);
}

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
    app_init_qrc();

    qputenv("QT_STYLE_OVERRIDE", "Fusion");
    qputenv("QML_DISABLE_DISK_CACHE", "true");

#ifdef Q_OS_LINUX
    if (qEnvironmentVariable("QT_QPA_PLATFORM") != "offscreen") {
        qputenv("QT_QPA_PLATFORMTHEME", "gtk3");
    }
#endif

    const char* appName;
    if (true /*MUVersion::unstable()*/) {
        appName  = "Audacity4Development";
    } else {
        appName  = "Audacity4";
    }

#ifdef Q_OS_WIN
    // NOTE: There are some problems with rendering the application window on some integrated graphics processors
    //       see https://github.com/musescore/MuseScore/issues/8270
    QCoreApplication::setAttribute(Qt::AA_UseOpenGLES);

    if (!qEnvironmentVariableIsSet("QT_OPENGL_BUGLIST")) {
        qputenv("QT_OPENGL_BUGLIST", ":/resources/win_opengl_buglist.json");
    }
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
    QCoreApplication::setOrganizationName("Audacity");
    QCoreApplication::setOrganizationDomain("audacityteam.org");
    // QCoreApplication::setApplicationVersion(QString::fromStdString(MUVersion::fullVersion().toStdString()));

// #if !defined(Q_OS_WIN) && !defined(Q_OS_DARWIN) && !defined(Q_OS_WASM)
//     // Any OS that uses Freedesktop.org Desktop Entry Specification (e.g. Linux, BSD)
//     QGuiApplication::setDesktopFileName("org.musescore.MuseScore" MU_APP_INSTALL_SUFFIX ".desktop");
// #endif

    commandLineParser.processBuiltinArgs(*app);

    // ====================================================
    // Setup modules: Resources, Exports, Imports, UiTypes
    // ====================================================
    globalModule.registerResources();
    globalModule.registerExports();
    globalModule.registerUiTypes();

    for (modularity::IModuleSetup* m : m_modules) {
        m->setApplication(muapplication());
        m->registerResources();
    }

    for (modularity::IModuleSetup* m : m_modules) {
        m->registerExports();
    }

    globalModule.resolveImports();
    globalModule.registerApi();
    for (modularity::IModuleSetup* m : m_modules) {
        m->registerUiTypes();
        m->resolveImports();
        m->registerApi();
    }

    // ====================================================
    // Setup modules: apply the command line options
    // ====================================================
    //! TODO Temporary fix
    dynamic_cast<muse::BaseApplication*>(muapplication().get())->setRunMode(runMode);
    applyCommandLineOptions(commandLineParser.options());

    // ====================================================
    // Setup modules: onPreInit
    // ====================================================
    globalModule.onPreInit(runMode);
    for (modularity::IModuleSetup* m : m_modules) {
        m->onPreInit(runMode);
    }

#ifdef AU_BUILD_APPSHELL_MODULE
    au::appshell::SplashScreen* splashScreen = nullptr;
    if (runMode == IApplication::RunMode::GuiApp) {
        //splashScreen = new SplashScreen(SplashScreen::Default);

        // if (multiInstancesProvider()->isMainInstance()) {
        //     splashScreen = new SplashScreen(SplashScreen::Default);
        // } else {
        //     const project::ProjectFile& file = startupScenario()->startupScoreFile();
        //     if (file.isValid()) {
        //         if (file.hasDisplayName()) {
        //             splashScreen = new SplashScreen(SplashScreen::ForNewInstance, false, file.displayName(true /* includingExtension */));
        //         } else {
        //             splashScreen = new SplashScreen(SplashScreen::ForNewInstance, false);
        //         }
        //     } else if (startupScenario()->isStartWithNewFileAsSecondaryInstance()) {
        //         splashScreen = new SplashScreen(SplashScreen::ForNewInstance, true);
        //     } else {
        //         splashScreen = new SplashScreen(SplashScreen::Default);
        //     }
        // }
    }

    if (splashScreen) {
        splashScreen->show();
    }
#endif

    // ====================================================
    // Setup modules: onInit
    // ====================================================
    globalModule.onInit(runMode);
    for (modularity::IModuleSetup* m : m_modules) {
        m->onInit(runMode);
    }

    // ====================================================
    // Setup modules: onAllInited
    // ====================================================
    globalModule.onAllInited(runMode);
    for (modularity::IModuleSetup* m : m_modules) {
        m->onAllInited(runMode);
    }

    // ====================================================
    // Setup modules: onStartApp (on next event loop)
    // ====================================================
    QMetaObject::invokeMethod(qApp, [this]() {
        globalModule.onStartApp();
        for (modularity::IModuleSetup* m : m_modules) {
            m->onStartApp();
        }
    }, Qt::QueuedConnection);

    // ====================================================
    // Run
    // ====================================================

    switch (runMode) {
    case IApplication::RunMode::ConsoleApp: {
        // // ====================================================
        // // Process Autobot
        // // ====================================================
        // CommandLineParser::Autobot autobot = commandLineParser.autobot();
        // if (!autobot.testCaseNameOrFile.isEmpty()) {
        //     QMetaObject::invokeMethod(qApp, [this, autobot]() {
        //             processAutobot(autobot);
        //         }, Qt::QueuedConnection);
        // } else {
        //     // ====================================================
        //     // Process Diagnostic
        //     // ====================================================
        //     CommandLineParser::Diagnostic diagnostic = commandLineParser.diagnostic();
        //     if (diagnostic.type != CommandLineParser::DiagnosticType::Undefined) {
        //         QMetaObject::invokeMethod(qApp, [this, diagnostic]() {
        //                 int code = processDiagnostic(diagnostic);
        //                 qApp->exit(code);
        //             }, Qt::QueuedConnection);
        //     } else {
        //         // ====================================================
        //         // Process Converter
        //         // ====================================================
        //         CommandLineParser::ConverterTask task = commandLineParser.converterTask();
        //         QMetaObject::invokeMethod(qApp, [this, task]() {
        //                 int code = processConverter(task);
        //                 qApp->exit(code);
        //             }, Qt::QueuedConnection);
        //     }
        // }
    } break;
    case IApplication::RunMode::GuiApp: {
#ifdef AU_BUILD_APPSHELL_MODULE
        // ====================================================
        // Setup Qml Engine
        // ====================================================
        QQmlApplicationEngine* engine = modularity::_ioc()->resolve<muse::ui::IUiEngine>("app")->qmlAppEngine();

#if defined(Q_OS_WIN)
        const QString mainQmlFile = "/platform/win/Main.qml";
#elif defined(Q_OS_MACOS)
        const QString mainQmlFile = "/platform/mac/Main.qml";
#elif defined(Q_OS_LINUX) || defined(Q_OS_FREEBSD)
        const QString mainQmlFile = "/platform/linux/Main.qml";
#elif defined(Q_OS_WASM)
        const QString mainQmlFile = "/Main.wasm.qml";
#endif

        const QUrl url(QStringLiteral("qrc:/qml") + mainQmlFile);

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

                    startupScenario()->runOnSplashScreen();

                    globalModule.onDelayedInit();
                    for (modularity::IModuleSetup* m : m_modules) {
                        m->onDelayedInit();
                    }

                    if (splashScreen) {
                        splashScreen->close();
                        delete splashScreen;
                    }

                    startupScenario()->runAfterSplashScreen();
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

#ifdef AU_BUILD_APPSHELL_MODULE
    // Engine quit
    modularity::_ioc()->resolve<muse::ui::IUiEngine>("app")->quit();
#endif

    // Deinit

    globalModule.invokeQueuedCalls();

    for (modularity::IModuleSetup* m : m_modules) {
        m->onDeinit();
    }

    globalModule.onDeinit();

    for (modularity::IModuleSetup* m : m_modules) {
        m->onDestroy();
    }

    globalModule.onDestroy();

    // Delete modules
    qDeleteAll(m_modules);
    m_modules.clear();
    modularity::_ioc()->reset();

    delete app;

    return retCode;
}

void App::applyCommandLineOptions(const CommandLineParser::Options& options)
{
    if (options.app.revertToFactorySettings) {
        appshellConfiguration()->revertToFactorySettings(options.app.revertToFactorySettings.value());
    }

    startupScenario()->setStartupType(options.startup.type);

    if (options.startup.scoreUrl.has_value()) {
        project::ProjectFile file { options.startup.scoreUrl.value() };

        if (options.startup.scoreDisplayNameOverride.has_value()) {
            file.displayNameOverride = options.startup.scoreDisplayNameOverride.value();
        }

        startupScenario()->setStartupScoreFile(file);
    }

    if (options.app.loggerLevel) {
        globalModule.setLoggerLevel(options.app.loggerLevel.value());
    }
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
