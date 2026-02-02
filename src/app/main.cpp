/*
 * Audacity: A Digital Audio Editor
 */

#include <QApplication>
#include <QStyleHints>
#include <QTextCodec>

#include <csignal>

#include "appfactory.h"
#include "commandlineparser.h"
#include "log.h"

#include "muse_framework_config.h"

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

    // Force the 8-bit text encoding to UTF-8. This is the default encoding on all supported platforms except for MSVC under Windows, which
    // would otherwise default to the local ANSI code page and cause corruption of any non-ANSI Unicode characters in command-line arguments.
    QTextCodec::setCodecForLocale(QTextCodec::codecForName("UTF-8"));

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

    // ====================================================
    // Create QApplication based on run mode
    // ====================================================
    QCoreApplication* qApplication = nullptr;

    if (commandLineParser.runMode() == muse::IApplication::RunMode::AudioPluginRegistration) {
        qApplication = new QCoreApplication(argcFinal, argvFinal);
    } else {
        qApplication = new QApplication(argcFinal, argvFinal);
    }

    commandLineParser.processBuiltinArgs(*qApplication);

    // ====================================================
    // Create and run the application
    // ====================================================
    au::app::AppFactory factory;
    std::shared_ptr<muse::IApplication> app = factory.newApp(commandLineParser);

    app->setup();

    app->setupNewContext();

    int code = qApplication->exec();

    app->finish();

    delete qApplication;

    LOGI() << "Goodbye!! code: " << code;
    return code;
}
