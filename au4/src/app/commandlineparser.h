/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QCommandLineParser>
#include <QStringList>
#include <optional>

#include "logger.h"

#include "global/iapplication.h"
#include "io/path.h"

class QCoreApplication;

namespace au::app {
class CommandLineParser
{
    //! NOTE: This parser is created at the earliest stage of the application initialization
    //! You should not inject anything into it
public:
    CommandLineParser() = default;

    struct Options {
        struct {
            std::optional<double> physicalDotsPerInch;
        } ui;

        struct {
            std::optional<bool> templateModeEnabled;
            std::optional<bool> testModeEnabled;
        } notation;

        struct {
            std::optional<bool> fullMigration;
        } project;

        struct {
            std::optional<int> trimMarginPixelSize;
            std::optional<float> pngDpiResolution;
        } exportImage;

        struct {
            std::optional<int> mp3Bitrate;
        } exportAudio;

        struct {
            std::optional<std::string> resolution;
            std::optional<int> fps;
            std::optional<double> leadingSec;
            std::optional<double> trailingSec;
        } exportVideo;

        struct {
            std::optional<muse::io::path_t> operationsFile;
        } importMidi;

        struct {
            std::optional<bool> linkedTabStaffCreated;
            std::optional<bool> experimental;
        } guitarPro;

        struct {
            std::optional<bool> revertToFactorySettings;
            std::optional<muse::logger::Level> loggerLevel;
        } app;

        struct {
            std::optional<std::string> type;
            std::optional<QUrl> scoreUrl;
            std::optional<QString> scoreDisplayNameOverride;
        } startup;
    };

    enum class ConvertType {
        File,
        Batch,
        ConvertScoreParts,
        ExportScoreMedia,
        ExportScoreMeta,
        ExportScoreParts,
        ExportScorePartsPdf,
        ExportScoreTranspose,
        SourceUpdate,
        ExportScoreVideo
    };

    enum class ParamKey {
        HighlightConfigPath,
        StylePath,
        ScoreSource,
        ScoreTransposeOptions,
        ForceMode,
        SoundProfile,

        // Video
    };

    struct ConverterTask {
        ConvertType type = ConvertType::File;

        QString inputFile;
        QString outputFile;

        QMap<ParamKey, QVariant> params;
    };

    enum class DiagnosticType {
        Undefined = 0,
        GenDrawData,
        ComDrawData,
        DrawDataToPng,
        DrawDiffToPng
    };

    struct Diagnostic {
        DiagnosticType type = DiagnosticType::Undefined;
        QStringList input;
        QString output;
    };

    struct Autobot {
        QString testCaseNameOrFile;
        QString testCaseContextNameOrFile;
        QString testCaseContextValue;
        QString testCaseFunc;
        QString testCaseFuncArgs;
    };

    struct AudioPluginRegistration {
        muse::io::path_t pluginPath;
        bool failedPlugin = false;
        int failCode = 0;
    };

    void init();
    void parse(int argc, char** argv);
    void processBuiltinArgs(const QCoreApplication& app);

    muse::IApplication::RunMode runMode() const;

    // Options
    const Options& options() const;

    // Tasks
    ConverterTask converterTask() const;
    Diagnostic diagnostic() const;
    Autobot autobot() const;
    AudioPluginRegistration audioPluginRegistration() const;

private:
    void printLongVersion() const;

    QCommandLineParser m_parser;
    muse::IApplication::RunMode m_runMode = muse::IApplication::RunMode::GuiApp;
    Options m_options;

    ConverterTask m_converterTask;
    Diagnostic m_diagnostic;
    Autobot m_autobot;
    AudioPluginRegistration m_audioPluginRegistration;
};
}
