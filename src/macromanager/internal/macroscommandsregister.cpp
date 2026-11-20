/*
* Audacity: A Digital Audio Editor
*/
#include "macroscommandsregister.h"

using namespace au::macromanager;
using namespace muse::rcommand;

std::string MacrosCommandsRegister::moduleName() const
{
    return "macromanager";
}

const std::vector<Command>& MacrosCommandsRegister::commandList() const
{
    return m_commands;
}

const std::vector<CommandInfo>& MacrosCommandsRegister::commandInfoList() const
{
    return m_commandInfos;
}
