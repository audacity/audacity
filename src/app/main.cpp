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

// Framework
#include "muse_framework_config.h"
#include "diagnostics/diagnosticsmodule.h"
#include "framework/draw/drawmodule.h"
#include "framework/actions/actionsmodule.h"
#include "framework/audioplugins/audiopluginsmodule.h"
#include "framework/ui/uimodule.h"
#include "framework/shortcuts/shortcutsmodule.h"
#include "framework/accessibility/accessibilitymodule.h"
#include "framework/uicomponents/uicomponentsmodule.h"
#include "framework/dockwindow/dockmodule.h"
#include "framework/cloud/cloudmodule.h"
#include "framework/network/networkmodule.h"
#include "framework/learn/learnmodule.h"
#include "framework/languages/languagesmodule.h"
#include "framework/workspace/workspacemodule.h"

// need stubs
#include "framework/stubs/multiinstances/multiinstancesstubmodule.h"

// -----
#include "appshell/appshellmodule.h"
#include "context/contextmodule.h"
#include "preferences/preferencesmodule.h"
#include "project/projectmodule.h"
#include "projectscene/projectscenemodule.h"
#include "audio/audiomodule.h"
#include "au3audio/au3audiomodule.h"
#include "playback/playbackmodule.h"
#include "trackedit/trackeditmodule.h"
#include "spectrogram/spectrogrammodule.h"
#include "record/recordmodule.h"
#include "uicomponents/uicomponentsmodule.h"
#include "toast/toastmodule.h"
#include "effects/effects_base/effectsmodule.h"
#include "effects/builtin/builtineffectsmodule.h"
#ifdef AU_MODULE_EFFECTS_LV2
#include "effects/lv2/lv2effectsmodule.h"
#else
#include "stubs/lv2/lv2effectsstubmodule.h"
#endif
#ifdef AU_MODULE_EFFECTS_VST
#include "effects/vst/vsteffectsmodule.h"
#else
#include "stubs/vst/vsteffectsstubmodule.h"
#endif
#ifdef AU_MODULE_EFFECTS_AUDIO_UNIT
#include "effects/audio_unit/audiouniteffectsmodule.h"
#else
#include "stubs/audio_unit/audiouniteffectsstubmodule.h"
#endif
#include "effects/nyquist/nyquisteffectsmodule.h"
#include "importexport/import/importermodule.h"
#include "importexport/export/exportermodule.h"
#include "importexport/labels/labelsmodule.h"

#ifdef MUSE_MODULE_AUTOBOT
#include "autobot/autobotmodule.h"
#endif

#ifdef MUSE_MODULE_EXTENSIONS
#include "framework/extensions/extensionsmodule.h"
#else
#include "framework/stubs/extensions/extensionsstubmodule.h"
#endif

#include "au3wrap/au3wrapmodule.h"

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

#ifndef MUSE_MODULE_DIAGNOSTICS_CRASHPAD_CLIENT
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
    if (qEnvironmentVariable("AU_QT_QPA_PLATFORM") != "offscreen") {
        qputenv("QT_QPA_PLATFORMTHEME", "gtk3");
    }

    //! NOTE Forced X11, with Wayland there are a number of problems now
    if (qEnvironmentVariable("AU_QT_QPA_PLATFORM") == "") {
        qputenv("QT_QPA_PLATFORM", "xcb");
    }
#endif

    const char* appName;
#ifdef MUSE_APP_UNSTABLE
    appName  = "Audacity4Development";
#else
    appName  = "Audacity4";
#endif

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
    QCoreApplication::setApplicationVersion(MUSE_APP_VERSION);

#if !defined(Q_OS_WIN) && !defined(Q_OS_DARWIN) && !defined(Q_OS_WASM)
#ifndef MUSE_APP_INSTALL_SUFFIX
#define MUSE_APP_INSTALL_SUFFIX ""
#endif
    // Any OS that uses Freedesktop.org Desktop Entry Specification (e.g. Linux, BSD)
    QGuiApplication::setDesktopFileName("org.audacityteam.Audacity" MUSE_APP_INSTALL_SUFFIX ".desktop");
#endif

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
    app.addModule(new muse::diagnostics::DiagnosticsModule());

// framework
    app.addModule(new muse::audioplugins::AudioPluginsModule());
    app.addModule(new muse::actions::ActionsModule());
    if (!isPluginRegistration) {
        app.addModule(new muse::draw::DrawModule());
        app.addModule(new muse::workspace::WorkspaceModule());
        app.addModule(new muse::accessibility::AccessibilityModule());
        app.addModule(new muse::mi::MultiInstancesModule());
        app.addModule(new muse::learn::LearnModule());
        app.addModule(new muse::languages::LanguagesModule());
        app.addModule(new muse::ui::UiModule());
        app.addModule(new muse::uicomponents::UiComponentsModule());
        app.addModule(new muse::dock::DockModule());
#ifdef MUSE_MODULE_SHORTCUTS
        app.addModule(new muse::shortcuts::ShortcutsModule());
#endif
        app.addModule(new muse::cloud::CloudModule());
        app.addModule(new muse::network::NetworkModule());
        app.addModule(new muse::extensions::ExtensionsModule());
#ifdef MUSE_MODULE_AUTOBOT
        app.addModule(new muse::autobot::AutobotModule());
#endif
    }

    // modules
    app.addModule(new au::appshell::AppShellModule());
    app.addModule(new au::preferences::PreferencesModule());
    app.addModule(new au::uicomponents::UiComponentsModule());
    app.addModule(new au::effects::AudioUnitEffectsModule());
    app.addModule(new au::effects::Lv2EffectsModule());
    app.addModule(new au::effects::VstEffectsModule());

    if (!isPluginRegistration) {
        app.addModule(new au::context::ContextModule());
        app.addModule(new au::audio::AudioModule());
        app.addModule(new au::au3audio::Au3AudioModule());
        app.addModule(new au::projectscene::ProjectSceneModule());
        app.addModule(new au::playback::PlaybackModule());
        app.addModule(new au::record::RecordModule());
        app.addModule(new au::trackedit::TrackeditModule());
        app.addModule(new au::spectrogram::SpectrogramModule());
        app.addModule(new au::toast::ToastModule());
        app.addModule(new au::project::ProjectModule());
        app.addModule(new au::importexport::ExporterModule());
        app.addModule(new au::importexport::ImporterModule());
        app.addModule(new au::importexport::LabelsModule());
        app.addModule(new au::au3::Au3WrapModule());
        app.addModule(new au::effects::EffectsModule());
        app.addModule(new au::effects::BuiltinEffectsModule());
        app.addModule(new au::effects::NyquistEffectsModule());
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
        qApplication = new QCoreApplication(argcFinal, argvFinal);
    } else {
        qApplication = new QApplication(argcFinal, argvFinal);
    }

    commandLineParser.processBuiltinArgs(*qApplication);

    int code = app.run(*qApplication, commandLineParser);

    delete qApplication;

    LOGI() << "Goodbye!! code: " << code;
    return code;
}
