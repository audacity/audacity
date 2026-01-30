/*
 * Audacity: A Digital Audio Editor
 */
#include "commandlineparser.h"

#include <QDir>

#include "global/io/dir.h"

#include "log.h"

using namespace au::app;
using namespace muse;

static QStringList prepareArguments(int argc, char** argv)
{
    QStringList args;

    for (int i = 0; i < argc; ++i) {
        QString arg = QString::fromLocal8Bit(argv[i]);

#ifndef NDEBUG
        if (arg.startsWith("-qmljsdebugger")) {
            continue;
        }
#endif

        args << arg;
    }

    return args;
}

template<typename ... Args>
QCommandLineOption internalCommandLineOption(Args&& ... args)
{
    QCommandLineOption option(std::forward<Args>(args)...);
    option.setFlags(QCommandLineOption::HiddenFromHelp);
    return option;
}

void CommandLineParser::init()
{
    // Common
    m_parser.addHelpOption(); // -?, -h, --help
    m_parser.addVersionOption(); // -v, --version

    m_parser.addPositionalArgument("projectfiles", "The files to open", "[file...]");

    m_parser.addOption(QCommandLineOption("long-version", "Print detailed version information"));
    m_parser.addOption(QCommandLineOption({ "d", "debug" }, "Debug mode"));

    m_parser.addOption(QCommandLineOption({ "D", "monitor-resolution" }, "Specify monitor resolution", "DPI"));

    m_parser.addOption(QCommandLineOption({ "F", "factory-settings" }, "Use factory settings"));
    m_parser.addOption(QCommandLineOption({ "R", "revert-settings" }, "Revert to factory settings, but keep default preferences"));

    m_parser.addOption(QCommandLineOption("session-type", "Startup with given session type", "type"));

    // Audio plugins
    m_parser.addOption(QCommandLineOption("register-audio-plugin",
                                          "Check an audio plugin for compatibility with the application and register it", "path"));
    m_parser.addOption(QCommandLineOption("register-failed-audio-plugin", "Register an incompatible audio plugin", "path"));

    // Autobot
    m_parser.addOption(QCommandLineOption("test-case", "Run test case by name or file", "nameOrFile"));
    m_parser.addOption(QCommandLineOption("test-case-context", "Set test case context by name or file", "nameOrFile"));
    m_parser.addOption(QCommandLineOption("test-case-context-value", "Set test case context value", "value"));
    m_parser.addOption(QCommandLineOption("test-case-func", "Call test case function", "name"));
    m_parser.addOption(QCommandLineOption("test-case-func-args", "Call test case function args", "args"));
}

void CommandLineParser::parse(int argc, char** argv)
{
    QStringList args = prepareArguments(argc, argv);
    m_parser.parse(args);

    auto floatValue = [this](const QString& name) -> std::optional<float> {
        bool ok = true;
        float val = m_parser.value(name).toFloat(&ok);
        if (ok) {
            return val;
        }
        return std::nullopt;
    };

    auto doubleValue = [this](const QString& name) -> std::optional<double> {
        bool ok = true;
        double val = m_parser.value(name).toDouble(&ok);
        if (ok) {
            return val;
        }
        return std::nullopt;
    };

    auto intValue = [this](const QString& name) -> std::optional<int> {
        bool ok = true;
        int val = m_parser.value(name).toInt(&ok);
        if (ok) {
            return val;
        }
        return std::nullopt;
    };

    auto fromUserInputPath = [](const QString& path) -> QString {
        return io::Dir::fromNativeSeparators(path).toQString();
    };

    QStringList projectfiles;
    for (const QString& arg : m_parser.positionalArguments()) {
        projectfiles << fromUserInputPath(arg);
    }

    if (m_parser.isSet("long-version")) {
        printLongVersion();
        exit(EXIT_SUCCESS);
    }

    if (m_parser.isSet("d")) {
        m_options.app.loggerLevel = muse::logger::Level::Debug;
    }

    if (m_parser.isSet("D")) {
        std::optional<double> val = doubleValue("D");
        if (val) {
            m_options.ui.physicalDotsPerInch = val;
        } else {
            LOGE() << "Option: -D not recognized DPI value: " << m_parser.value("D");
        }
    }

    if (m_parser.isSet("session-type")) {
        m_options.startup.type = m_parser.value("session-type").toStdString();
    }

    if (m_parser.isSet("F") || m_parser.isSet("R")) {
        m_options.app.revertToFactorySettings = true;
    }

    // Audio plugin registration
    if (m_parser.isSet("register-audio-plugin")) {
        m_runMode = IApplication::RunMode::AudioPluginRegistration;
        m_audioPluginRegistration.pluginPath = fromUserInputPath(m_parser.value("register-audio-plugin"));
        m_audioPluginRegistration.failedPlugin = false;
    }

    if (m_parser.isSet("register-failed-audio-plugin")) {
        QStringList args1 = m_parser.positionalArguments();
        m_runMode = IApplication::RunMode::AudioPluginRegistration;
        m_audioPluginRegistration.pluginPath = fromUserInputPath(m_parser.value("register-failed-audio-plugin"));
        m_audioPluginRegistration.failedPlugin = true;
        m_audioPluginRegistration.failCode = !args1.empty() ? args1[0].toInt() : -1;
    }

    // Autobot
    if (m_parser.isSet("test-case")) {
        m_runMode = IApplication::RunMode::ConsoleApp;
        m_autobot.testCaseNameOrFile = fromUserInputPath(m_parser.value("test-case"));
    }

    if (m_parser.isSet("test-case-context")) {
        m_autobot.testCaseContextNameOrFile = fromUserInputPath(m_parser.value("test-case-context"));
    }

    if (m_parser.isSet("test-case-context-value")) {
        m_autobot.testCaseContextValue = m_parser.value("test-case-context-value");
    }

    if (m_parser.isSet("test-case-func")) {
        m_autobot.testCaseFunc = m_parser.value("test-case-func");
    }

    if (m_parser.isSet("test-case-func-args")) {
        m_autobot.testCaseFuncArgs = m_parser.value("test-case-func-args");
    }

    // Startup
    if (m_runMode == IApplication::RunMode::GuiApp) {
        if (!projectfiles.isEmpty()) {
            m_options.startup.projectUrl = QUrl::fromUserInput(projectfiles[0], QDir::currentPath(), QUrl::AssumeLocalFile);
        }
    }
}

void CommandLineParser::processBuiltinArgs(const QCoreApplication& app)
{
    //! NOTE: some options require an instance of QCoreApplication
    m_parser.process(app);
}

muse::IApplication::RunMode CommandLineParser::runMode() const
{
    return m_runMode;
}

const CommandLineParser::Options& CommandLineParser::options() const
{
    return m_options;
}

CommandLineParser::Autobot CommandLineParser::autobot() const
{
    return m_autobot;
}

CommandLineParser::AudioPluginRegistration CommandLineParser::audioPluginRegistration() const
{
    return m_audioPluginRegistration;
}

void CommandLineParser::printLongVersion() const
{
    NOT_IMPLEMENTED;
}
