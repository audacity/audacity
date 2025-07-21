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
#include "project/projectmodule.h"
#include "projectscene/projectscenemodule.h"
#include "au3audio/audiomodule.h"
#include "playback/playbackmodule.h"
#include "trackedit/trackeditmodule.h"
#include "record/recordmodule.h"
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
    if (!isPluginRegistration) {
        app.addModule(new muse::draw::DrawModule());
        app.addModule(new muse::actions::ActionsModule());
        app.addModule(new muse::workspace::WorkspaceModule());
        app.addModule(new muse::accessibility::AccessibilityModule());
        app.addModule(new muse::mi::MultiInstancesModule());
        app.addModule(new muse::learn::LearnModule());
        app.addModule(new muse::languages::LanguagesModule());
        app.addModule(new muse::ui::UiModule());
        app.addModule(new muse::uicomponents::UiComponentsModule());
        app.addModule(new muse::dock::DockModule());
        app.addModule(new muse::shortcuts::ShortcutsModule());
        app.addModule(new muse::cloud::CloudModule());
        app.addModule(new muse::network::NetworkModule());
    }

    // modules
    app.addModule(new au::appshell::AppShellModule());
    app.addModule(new muse::extensions::ExtensionsModule());
#ifdef MUSE_MODULE_AUTOBOT
    app.addModule(new muse::autobot::AutobotModule());
#endif
    app.addModule(new au::effects::AudioUnitEffectsModule());
    app.addModule(new au::effects::Lv2EffectsModule());
    app.addModule(new au::effects::VstEffectsModule());
    app.addModule(new au::importexport::ExporterModule());
    app.addModule(new au::importexport::ImporterModule());

    if (!isPluginRegistration) {
        app.addModule(new au::context::ContextModule());
        app.addModule(new au::audio::AudioModule());
        app.addModule(new au::projectscene::ProjectSceneModule());
        app.addModule(new au::playback::PlaybackModule());
        app.addModule(new au::record::RecordModule());
        app.addModule(new au::trackedit::TrackeditModule());
        app.addModule(new au::project::ProjectModule());
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
        qApplication = new QCoreApplication(argc, argv);
    } else {
        qApplication = new QApplication(argc, argv);
    }

    int code = app.run(*qApplication, commandLineParser);

    delete qApplication;

    LOGI() << "Goodbye!! code: " << code;
    return code;
}
