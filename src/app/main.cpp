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

#include <QApplication>
#include <QStyleHints>
#include <QTextCodec>

#include <csignal>

#include "app.h"
#include "commandlineparser.h"
#include "log.h"
#include "neededmodulesetups.h"

#if (defined (_MSCVER) || defined (_MSC_VER))
#include <vector>
#include <algorithm>
#include <windows.h>
#include <shellapi.h>
#endif

static void app_init_qrc()
{
    Q_INIT_RESOURCE(app);
}

#ifndef MU_BUILD_CRASHPAD_CLIENT
static void crashCallback(int signum)
{
    const char* signame = "UNKNOWN SIGNAME";
    const char* sigdescript = "";
    switch (signum) {
    case SIGILL:
        signame = "SIGILL";
        sigdescript = "Illegal Instruction";
        break;
    case SIGSEGV:
        signame = "SIGSEGV";
        sigdescript =  "Invalid memory reference";
        break;
    }
    LOGE() << "Oops! Application crashed with signal: [" << signum << "] " << signame << "-" << sigdescript;
    exit(EXIT_FAILURE);
}

#endif

#ifdef Q_OS_WIN
#include <crtdbg.h> // _CrtSetDbgFlag
#endif

int main(int argc, char** argv)
{
#ifdef Q_OS_WIN
    // Configure memory leak detection in debug builds.
    // At the moment we keep it disabled because it causes shutdown to take about 10 seconds.
    // A ticket was logged to investigate this further: https://github.com/audacity/audacity/issues/7568
    constexpr auto enableMemoryLeakReport = false;
    _CrtSetDbgFlag(enableMemoryLeakReport ? _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF : _CRTDBG_ALLOC_MEM_DF);
#endif

#ifndef MUE_BUILD_CRASHPAD_CLIENT
    signal(SIGSEGV, crashCallback);
    signal(SIGILL, crashCallback);
    signal(SIGFPE, crashCallback);
#endif

    // ====================================================
    // Setup global Qt application variables
    // ====================================================
    app_init_qrc();

    qputenv("QT_STYLE_OVERRIDE", "Fusion");
    qputenv("QML_DISABLE_DISK_CACHE", "true");

    // HACK: Workaround for crash MuseScore #28840. This disables the incremental GC
    if (!qEnvironmentVariableIsSet("QV4_GC_TIMELIMIT")) {
        qputenv("QV4_GC_TIMELIMIT", "0");
    }

#if QT_VERSION >= QT_VERSION_CHECK(6, 6, 0)
    if (!qEnvironmentVariableIsSet("QT_QUICK_FLICKABLE_WHEEL_DECELERATION")) {
        qputenv("QT_QUICK_FLICKABLE_WHEEL_DECELERATION", "5000");
    }
#endif

#ifdef Q_OS_LINUX
    if (qEnvironmentVariable("QT_QPA_PLATFORM") != "offscreen") {
        // At the time of writing the app hangs when started with "gtk3" theme
        // TODO: #9308
        qputenv("QT_QPA_PLATFORMTHEME", "qt6ct");
    }

    //! NOTE Forced X11, with Wayland there are a number of problems now
    if (qEnvironmentVariable("QT_QPA_PLATFORM") == "") {
        qputenv("QT_QPA_PLATFORM", "xcb");
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
    QGuiApplication::styleHints()->setMousePressAndHoldInterval(250);

    QCoreApplication::setApplicationName(appName);
    QCoreApplication::setOrganizationName("Audacity");
    QCoreApplication::setOrganizationDomain("audacityteam.org");
    // QCoreApplication::setApplicationVersion(QString::fromStdString(MUVersion::fullVersion().toStdString()));

// #if !defined(Q_OS_WIN) && !defined(Q_OS_DARWIN) && !defined(Q_OS_WASM)
//     // Any OS that uses Freedesktop.org Desktop Entry Specification (e.g. Linux, BSD)
//     QGuiApplication::setDesktopFileName("org.musescore.MuseScore" MU_APP_INSTALL_SUFFIX ".desktop");
// #endif

    // ====================================================
    // Parse command line options
    // ====================================================
    au::app::CommandLineParser commandLineParser;
    commandLineParser.init();
    commandLineParser.parse(argc, argv);

    const bool isPluginRegistration = commandLineParser.runMode() == muse::IApplication::RunMode::AudioPluginRegistration;

    // Force the 8-bit text encoding to UTF-8. This is the default encoding on all supported platforms except for MSVC under Windows, which
    // would otherwise default to the local ANSI code page and cause corruption of any non-ANSI Unicode characters in command-line arguments.
    QTextCodec::setCodecForLocale(QTextCodec::codecForName("UTF-8"));

    au::app::App app;

    //! NOTE `diagnostics` must be first, because it installs the crash handler.
    //! For other modules, the order is (an should be) unimportant.
    app.addModule(new au::NeededDiagnosticsModule());

// framework
    app.addModule(new au::NeededAudioPluginsModule());
    if (!isPluginRegistration) {
        app.addModule(new au::NeededDrawModule());
        app.addModule(new au::NeededActionsModule());
        app.addModule(new au::NeededWorkspaceModule());
        app.addModule(new au::NeededAccessibilityModule());
        app.addModule(new au::NeededMultiInstancesModule());
        app.addModule(new au::NeededLearnModule());
        app.addModule(new au::NeededLanguagesModule());
        app.addModule(new au::NeededUiModule());
        app.addModule(new au::NeededUiComponentsModule());
        app.addModule(new au::NeededDockModule());
        app.addModule(new au::NeededShortcutsModule());
        app.addModule(new au::NeededCloudModule());
        app.addModule(new au::NeededNetworkModule());
    }

    // modules
    app.addModule(new au::NeededAppShellModule());
    app.addModule(new au::NeededAudioUnitEffectsModule());
    app.addModule(new au::NeededLv2EffectsModule());
    app.addModule(new au::NeededVstEffectsModule());

    if (!isPluginRegistration) {
        app.addModule(new au::NeededExtensionsModule());
#ifdef MUSE_MODULE_AUTOBOT
        app.addModule(new au::NeededAutobotModule());
#endif
        app.addModule(new au::NeededContextModule());
        app.addModule(new au::NeededAudioModule());
        app.addModule(new au::NeededProjectSceneModule());
        app.addModule(new au::NeededPlaybackModule());
        app.addModule(new au::NeededRecordModule());
        app.addModule(new au::NeededTrackeditModule());
        app.addModule(new au::NeededProjectModule());
        app.addModule(new au::NeededExporterModule());
        app.addModule(new au::NeededImporterModule());
        app.addModule(new au::NeededAu3WrapModule());
        app.addModule(new au::NeededEffectsModule());
        app.addModule(new au::NeededBuiltinEffectsModule());
        app.addModule(new au::NeededNyquistEffectsModule());
    }

#if (defined (_MSCVER) || defined (_MSC_VER))
    // On MSVC under Windows, we need to manually retrieve the command-line arguments and convert them from UTF-16 to UTF-8.
    // This prevents data loss if there are any characters that wouldn't fit in the local ANSI code page.
    int argcUTF16 = 0;
    LPWSTR* argvUTF16 = CommandLineToArgvW(GetCommandLineW(), &argcUTF16);

    std::vector<QByteArray> argvUTF8Q;
    std::for_each(argvUTF16, argvUTF16 + argcUTF16, [&argvUTF8Q](const auto& arg) {
        argvUTF8Q.emplace_back(QString::fromUtf16(reinterpret_cast<const char16_t*>(arg), -1).toUtf8());
    });

    LocalFree(argvUTF16);

    std::vector<char*> argvUTF8;
    for (auto& arg : argvUTF8Q) {
        argvUTF8.push_back(arg.data());
    }

    // Don't use the arguments passed to main(), because they're in the local ANSI code page.
    Q_UNUSED(argc);
    Q_UNUSED(argv);

    int argcFinal = argcUTF16;
    char** argvFinal = argvUTF8.data();
#else

    int argcFinal = argc;
    char** argvFinal = argv;

#endif

    QCoreApplication* qApplication = nullptr;

    if (commandLineParser.runMode() == muse::IApplication::RunMode::AudioPluginRegistration) {
        qApplication = new QCoreApplication(argc, argv);
    } else {
        qApplication = new QApplication(argc, argv);
    }

    commandLineParser.processBuiltinArgs(*qApplication);

    int code = app.run(*qApplication, commandLineParser);

    delete qApplication;

    LOGI() << "Goodbye!! code: " << code;
    return code;
}
