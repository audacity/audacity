/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "framework/rcommand/imodulecommandsregister.h"

namespace au::macromanager {
class MacrosCommandsRegister : public muse::rcommand::IModuleCommandsRegister
{
public:
    std::string moduleName() const override;

    const std::vector<muse::rcommand::Command>& commandList() const override;
    const std::vector<muse::rcommand::CommandInfo>& commandInfoList() const override;

private:
    std::vector<muse::rcommand::Command> m_commands;
    std::vector<muse::rcommand::CommandInfo> m_commandInfos;
};
}
