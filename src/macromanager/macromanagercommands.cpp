/*
* Audacity: A Digital Audio Editor
*/
#include "macromanagercommands.h"

#include <QUrl>

namespace {
const std::string MACROS_SCHEME("macros");
const std::string MACRO_RUN_COMMAND("run");
}

muse::rcommand::Command au::macromanager::makeMacroRunCommand(const std::string& macroName)
{
    muse::rcommand::Command command;
    command.setScheme(std::string(muse::rcommand::COMMAND_SCHEME));
    command.addPath(MACROS_SCHEME);
    command.addPath(MACRO_RUN_COMMAND);
    command.addPath(QUrl::toPercentEncoding(QString::fromStdString(macroName)).toStdString());
    return command;
}
