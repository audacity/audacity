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

    m_parser.addPositionalArgument("scorefiles", "The files to open", "[scorefile...]");

    m_parser.addOption(QCommandLineOption("long-version", "Print detailed version information"));
    m_parser.addOption(QCommandLineOption({ "d", "debug" }, "Debug mode"));

    m_parser.addOption(QCommandLineOption({ "D", "monitor-resolution" }, "Specify monitor resolution", "DPI"));
    m_parser.addOption(QCommandLineOption({ "T", "trim-image" },
                                          "Use with '-o <file>.png' and '-o <file.svg>'. Trim exported image with specified margin (in pixels)",
                                          "margin"));

    m_parser.addOption(QCommandLineOption({ "b", "bitrate" }, "Use with '-o <file>.mp3', sets bitrate, in kbps", "bitrate"));

    m_parser.addOption(QCommandLineOption("template-mode", "Save template mode, no page size")); // and no platform and creationDate tags
    m_parser.addOption(QCommandLineOption({ "t", "test-mode" }, "Set test mode flag for all files")); // this includes --template-mode

    m_parser.addOption(QCommandLineOption("session-type", "Startup with given session type", "type")); // see StartupScenario::sessionTypeTromString

    // Converter mode
    m_parser.addOption(QCommandLineOption({ "r", "image-resolution" }, "Set output resolution for image export", "DPI"));
    m_parser.addOption(QCommandLineOption({ "j", "job" }, "Process a conversion job", "file"));
    m_parser.addOption(QCommandLineOption({ "o", "export-to" }, "Export to 'file'. Format depends on file's extension", "file"));
    m_parser.addOption(QCommandLineOption({ "F", "factory-settings" }, "Use factory settings"));
    m_parser.addOption(QCommandLineOption({ "R", "revert-settings" }, "Revert to factory settings, but keep default preferences"));
    m_parser.addOption(QCommandLineOption({ "M", "midi-operations" }, "Specify MIDI import operations file", "file"));
    m_parser.addOption(QCommandLineOption({ "P", "export-score-parts" }, "Use with '-o <file>.pdf', export score and parts"));
    m_parser.addOption(QCommandLineOption({ "f", "force" },
                                          "Use with '-o <file>', ignore warnings reg. score being corrupted or from wrong version"));

    m_parser.addOption(QCommandLineOption("score-media",
                                          "Export all media (excepting mp3) for a given score in a single JSON file and print it to stdout"));
    m_parser.addOption(QCommandLineOption("highlight-config", "Set highlight to svg, generated from a given score", "highlight-config"));
    m_parser.addOption(QCommandLineOption("score-meta", "Export score metadata to JSON document and print it to stdout"));
    m_parser.addOption(QCommandLineOption("score-parts", "Generate parts data for the given score and save them to separate mscz files"));
    m_parser.addOption(QCommandLineOption("score-parts-pdf",
                                          "Generate parts data for the given score and export the data to a single JSON file, print it to stdout"));
    m_parser.addOption(QCommandLineOption("score-transpose",
                                          "Transpose the given score and export the data to a single JSON file, print it to stdout",
                                          "options"));
    m_parser.addOption(QCommandLineOption("source-update", "Update the source in the given score"));

    m_parser.addOption(QCommandLineOption({ "S", "style" }, "Load style file", "style"));

    m_parser.addOption(QCommandLineOption("sound-profile",
                                          "Use with '-o <file>.mp3' or with '-j <file>', override the sound profile in the given score(s). "
                                          "Possible values: \"MuseScore Basic\", \"Muse Sounds\"", "sound-profile"));

    // Video export
#ifdef MUE_BUILD_VIDEOEXPORT_MODULE
    m_parser.addOption(QCommandLineOption("score-video", "Generate video for the given score and export it to file"));
// not implemented
//    m_parser.addOption(QCommandLineOption("view-mode",
//                                          "View mode [paged-float, paged-original, paged-float-height, pano, auto]. Auto (default) will choose the best mode according to number of instruments etc... Will show piano for piano score only",
//                                          "auto"));
// not implemented
//    m_parser.addOption(QCommandLineOption("piano", "Show Piano, works only if one part and not auto or float modes"));
//    m_parser.addOption(QCommandLineOption("piano-position", "Show Piano top or bottom. Default bottom", "bottom"));
    m_parser.addOption(QCommandLineOption("resolution", "Resolution [2160p, 1440p, 1080p, 720p, 480p, 360p]", "1080p"));
    m_parser.addOption(QCommandLineOption("fps", "Frame per second [60, 30, 24]", "24"));
    m_parser.addOption(QCommandLineOption("ls", "Pause before playback in seconds (3.0)", "3.0"));
    m_parser.addOption(QCommandLineOption("ts", "Pause before end of video in seconds (3.0)", "3.0"));
#endif

    m_parser.addOption(QCommandLineOption("gp-linked", "create tabulature linked staves for guitar pro"));
    m_parser.addOption(QCommandLineOption("gp-experimental", "experimental features for guitar pro import"));

    //! NOTE Currently only implemented `full` mode
    m_parser.addOption(QCommandLineOption("migration", "Whether to do migration with given mode, `full` - full migration", "mode"));

    // Diagnostic
    m_parser.addOption(QCommandLineOption("diagnostic-output", "Diagnostic output", "output"));
    m_parser.addOption(QCommandLineOption("diagnostic-gen-drawdata", "Generate engraving draw data", "scores-dir"));
    m_parser.addOption(QCommandLineOption("diagnostic-com-drawdata", "Compare engraving draw data"));
    m_parser.addOption(QCommandLineOption("diagnostic-drawdata-to-png", "Convert draw data to png", "file"));
    m_parser.addOption(QCommandLineOption("diagnostic-drawdiff-to-png", "Convert draw diff to png"));

    // Autobot
    m_parser.addOption(QCommandLineOption("test-case", "Run test case by name or file", "nameOrFile"));
    m_parser.addOption(QCommandLineOption("test-case-context", "Set test case context by name or file", "nameOrFile"));
    m_parser.addOption(QCommandLineOption("test-case-context-value", "Set test case context value", "value"));
    m_parser.addOption(QCommandLineOption("test-case-func", "Call test case function", "name"));
    m_parser.addOption(QCommandLineOption("test-case-func-args", "Call test case function args", "args"));

    // Audio plugins
    m_parser.addOption(QCommandLineOption("register-audio-plugin",
                                          "Check an audio plugin for compatibility with the application and register it", "path"));
    m_parser.addOption(QCommandLineOption("register-failed-audio-plugin", "Register an incompatible audio plugin", "path"));

    // Internal
    m_parser.addOption(internalCommandLineOption("score-display-name-override",
                                                 "Display name to be shown in splash screen for the score that is being opened", "name"));
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

    QStringList scorefiles;
    for (const QString& arg : m_parser.positionalArguments()) {
        scorefiles << fromUserInputPath(arg);
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

    if (m_parser.isSet("T")) {
        std::optional<int> val = intValue("T");
        if (val) {
            m_options.exportImage.trimMarginPixelSize = val;
        } else {
            LOGE() << "Option: -T not recognized trim value: " << m_parser.value("T");
        }
    }

    if (m_parser.isSet("M")) {
        m_options.importMidi.operationsFile = fromUserInputPath(m_parser.value("M"));
    }

    if (m_parser.isSet("b")) {
        std::optional<int> val = intValue("b");
        if (val) {
            m_options.exportAudio.mp3Bitrate = val;
        } else {
            LOGE() << "Option: -b not recognized bitrate value: " << m_parser.value("b");
        }
    }

    if (m_parser.isSet("template-mode")) {
        m_options.notation.templateModeEnabled = true;
    }

    if (m_parser.isSet("t")) {
        m_options.notation.testModeEnabled = true;
    }

    if (m_parser.isSet("session-type")) {
        m_options.startup.type = m_parser.value("session-type").toStdString();
    }

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

    // Converter mode
    if (m_parser.isSet("r")) {
        std::optional<float> val = floatValue("r");
        if (val) {
            m_options.exportImage.pngDpiResolution = val;
        } else {
            LOGE() << "Option: -r not recognized DPI value: " << m_parser.value("r");
        }
    }

    if (m_parser.isSet("o")) {
        m_runMode = IApplication::RunMode::ConsoleApp;
        m_converterTask.type = ConvertType::File;
        if (scorefiles.size() < 1) {
            LOGE() << "Option: -o no input file specified";
        } else {
            if (scorefiles.size() > 1) {
                LOGW() << "Option: -o multiple input files specified; processing only the first one";
            }
            m_converterTask.inputFile = scorefiles[0];
            m_converterTask.outputFile = fromUserInputPath(m_parser.value("o"));
        }
    }

    if (m_parser.isSet("P")) {
        if (m_converterTask.outputFile.isEmpty()) {
            LOGE() << "Option: -R no output file specified";
        } else {
            m_converterTask.type = ConvertType::ConvertScoreParts;
        }
    }

    if (m_parser.isSet("j")) {
        m_runMode = IApplication::RunMode::ConsoleApp;
        m_converterTask.type = ConvertType::Batch;
        m_converterTask.inputFile = fromUserInputPath(m_parser.value("j"));
    }

    if (m_parser.isSet("score-media")) {
        m_runMode = IApplication::RunMode::ConsoleApp;
        m_converterTask.type = ConvertType::ExportScoreMedia;
        m_converterTask.inputFile = scorefiles[0];
        if (m_parser.isSet("highlight-config")) {
            m_converterTask.params[CommandLineParser::ParamKey::HighlightConfigPath]
                = fromUserInputPath(m_parser.value("highlight-config"));
        }
    }

    if (m_parser.isSet("score-meta")) {
        m_runMode = IApplication::RunMode::ConsoleApp;
        m_converterTask.type = ConvertType::ExportScoreMeta;
        m_converterTask.inputFile = scorefiles[0];
    }

    if (m_parser.isSet("score-parts")) {
        m_runMode = IApplication::RunMode::ConsoleApp;
        m_converterTask.type = ConvertType::ExportScoreParts;
        m_converterTask.inputFile = scorefiles[0];
    }

    if (m_parser.isSet("score-parts-pdf")) {
        m_runMode = IApplication::RunMode::ConsoleApp;
        m_converterTask.type = ConvertType::ExportScorePartsPdf;
        m_converterTask.inputFile = scorefiles[0];
    }

    if (m_parser.isSet("score-transpose")) {
        m_runMode = IApplication::RunMode::ConsoleApp;
        m_converterTask.type = ConvertType::ExportScoreTranspose;
        m_converterTask.inputFile = scorefiles[0];
        m_converterTask.params[CommandLineParser::ParamKey::ScoreTransposeOptions] = m_parser.value("score-transpose");
    }

    if (m_parser.isSet("source-update")) {
        QStringList args2 = m_parser.positionalArguments();

        m_runMode = IApplication::RunMode::ConsoleApp;
        m_converterTask.type = ConvertType::SourceUpdate;
        m_converterTask.inputFile = fromUserInputPath(args2[0]);

        if (args2.size() >= 2) {
            m_converterTask.params[CommandLineParser::ParamKey::ScoreSource] = args2[1];
        } else {
            LOGW() << "Option: --source-update no source specified";
        }
    }

    // Video
#ifdef MUE_BUILD_VIDEOEXPORT_MODULE
    if (m_parser.isSet("score-video")) {
        m_runMode = IApplication::RunMode::ConsoleApp;
        m_converterTask.type = ConvertType::ExportScoreVideo;
        m_converterTask.inputFile = scorefiles[0];
        m_converterTask.outputFile = fromUserInputPath(m_parser.value("o"));

        if (m_parser.isSet("view-mode")) {
            NOT_IMPLEMENTED;
        }

        if (m_parser.isSet("piano")) {
            NOT_IMPLEMENTED;
        }

        if (m_parser.isSet("piano-position")) {
            NOT_IMPLEMENTED;
        }

        if (m_parser.isSet("resolution")) {
            m_options.exportVideo.resolution = m_parser.value("resolution").toStdString();
        }

        if (m_parser.isSet("fps")) {
            m_options.exportVideo.fps = intValue("fps");
        }

        if (m_parser.isSet("ls")) {
            m_options.exportVideo.leadingSec = doubleValue("ls");
        }

        if (m_parser.isSet("ts")) {
            m_options.exportVideo.trailingSec = doubleValue("ts");
        }
    }
#endif

    if (m_parser.isSet("F") || m_parser.isSet("R")) {
        m_options.app.revertToFactorySettings = true;
    }

    if (m_parser.isSet("f")) {
        m_converterTask.params[CommandLineParser::ParamKey::ForceMode] = true;
    }

    if (m_parser.isSet("S")) {
        m_converterTask.params[CommandLineParser::ParamKey::StylePath] = fromUserInputPath(m_parser.value("S"));
    }

    if (m_parser.isSet("sound-profile")) {
        m_converterTask.params[CommandLineParser::ParamKey::SoundProfile] = m_parser.value("sound-profile");
    }

    if (m_parser.isSet("gp-linked")) {
        m_options.guitarPro.linkedTabStaffCreated = true;
    }

    if (m_parser.isSet("gp-experimental")) {
        m_options.guitarPro.experimental = true;
    }

    if (m_runMode == IApplication::RunMode::ConsoleApp) {
        if (m_parser.isSet("migration")) {
            QString val = m_parser.value("migration");
            m_options.project.fullMigration = (val == "full") ? true : false;
        }
    }

    // Diagnostic
    if (m_parser.isSet("diagnostic-output")) {
        m_diagnostic.output = m_parser.value("diagnostic-output");
    }

    if (m_parser.isSet("diagnostic-gen-drawdata")) {
        m_runMode = IApplication::RunMode::ConsoleApp;
        m_diagnostic.type = DiagnosticType::GenDrawData;
        m_diagnostic.input << m_parser.value("diagnostic-gen-drawdata");
    }

    if (m_parser.isSet("diagnostic-com-drawdata")) {
        m_runMode = IApplication::RunMode::ConsoleApp;
        m_diagnostic.type = DiagnosticType::ComDrawData;
        m_diagnostic.input = scorefiles;
    }

    if (m_parser.isSet("diagnostic-drawdata-to-png")) {
        m_runMode = IApplication::RunMode::ConsoleApp;
        m_diagnostic.type = DiagnosticType::DrawDataToPng;
        m_diagnostic.input << m_parser.value("diagnostic-drawdata-to-png");
    }

    if (m_parser.isSet("diagnostic-drawdiff-to-png")) {
        m_runMode = IApplication::RunMode::ConsoleApp;
        m_diagnostic.type = DiagnosticType::DrawDiffToPng;
        m_diagnostic.input = scorefiles;
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
        if (!scorefiles.isEmpty()) {
            m_options.startup.scoreUrl = QUrl::fromUserInput(scorefiles[0], QDir::currentPath(), QUrl::AssumeLocalFile);
        }

        if (m_parser.isSet("score-display-name-override")) {
            m_options.startup.scoreDisplayNameOverride = m_parser.value("score-display-name-override");
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

CommandLineParser::ConverterTask CommandLineParser::converterTask() const
{
    return m_converterTask;
}

CommandLineParser::Diagnostic CommandLineParser::diagnostic() const
{
    return m_diagnostic;
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

    // if (MUVersion::unstable()) {
    //     printf("MuseScore: Music Score Editor\nUnstable Prerelease for Version %s; Build %s\n",
    //            MUVersion::version().toStdString().c_str(), MUVersion::revision().toStdString().c_str());
    // } else {
    //     printf("MuseScore: Music Score Editor; Version %s; Build %s\n",
    //            MUVersion::version().toStdString().c_str(), MUVersion::revision().toStdString().c_str());
    // }
}
