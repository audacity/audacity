/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QCommandLineParser>
#include <QStringList>
#include <optional>

#include "framework/global/logger.h"
#include "framework/global/iapplication.h"
#include "framework/global/io/path.h"

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
            std::optional<bool> revertToFactorySettings;
            std::optional<muse::logger::Level> loggerLevel;
        } app;

        struct {
            std::optional<std::string> type;
            std::optional<QUrl> projectUrl;
            std::optional<QString> projectDisplayNameOverride;
        } startup;
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
        bool selfTest = false;
    };

    void init();
    void parse(int argc, char** argv);
    void processBuiltinArgs(const QCoreApplication& app);

    muse::IApplication::RunMode runMode() const;

    // Options
    const Options& options() const;

    // Tasks
    Autobot autobot() const;
    AudioPluginRegistration audioPluginRegistration() const;

private:
    void printLongVersion() const;

    QCommandLineParser m_parser;
    muse::IApplication::RunMode m_runMode = muse::IApplication::RunMode::GuiApp;
    Options m_options;

    Autobot m_autobot;
    AudioPluginRegistration m_audioPluginRegistration;
};
}
