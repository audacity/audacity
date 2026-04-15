/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include <QCommandLineParser>
#include <QStringList>

#include "framework/global/logger.h"
#include "framework/global/iapplication.h"

#include "cmdoptions.h"

class QCoreApplication;

namespace au::app {
class CommandLineParser
{
    //! NOTE: This parser is created at the earliest stage of the application initialization
    //! You should not inject anything into it
public:
    CommandLineParser() = default;

    void init();
    void parse(int argc, char** argv);
    void processBuiltinArgs(const QCoreApplication& app);

    muse::IApplication::RunMode runMode() const;

    std::shared_ptr<AudacityCmdOptions> options() const;

private:
    void printLongVersion() const;

    QCommandLineParser m_parser;
    const std::shared_ptr<AudacityCmdOptions> m_options = std::make_shared<AudacityCmdOptions>();
};
}
