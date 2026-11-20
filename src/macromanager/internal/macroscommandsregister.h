/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/rcommand/imodulecommandsregister.h"

#include "../imacrosrepository.h"

namespace au::macromanager {
class MacrosCommandsRegister : public muse::rcommand::IModuleCommandsRegister, public muse::async::Asyncable
{
    muse::GlobalInject<IMacrosRepository> repository;

public:
    void init();

    std::string moduleName() const override;

    const std::vector<muse::rcommand::Command>& commandList() const override;
    const std::vector<muse::rcommand::CommandInfo>& commandInfoList() const override;
    muse::async::Notification commandListChanged() const override;

private:
    void reload();

    std::vector<muse::rcommand::Command> m_commands;
    std::vector<muse::rcommand::CommandInfo> m_commandInfos;
    muse::async::Notification m_commandListChanged;
};
}
