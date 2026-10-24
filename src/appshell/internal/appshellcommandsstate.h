/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <map>

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/rcommand/icommandsregister.h"
#include "framework/rcommand/imodulecommandsstate.h"
#include "framework/ui/imainwindow.h"

#include "context/iuicontextresolver.h"
#include "record/irecordcontroller.h"

namespace au::appshell {
class AppShellCommandsState : public muse::rcommand::IModuleCommandsState, public muse::Contextable, public muse::async::Asyncable
{
    muse::GlobalInject<muse::rcommand::ICommandsRegister> commandsRegister;
    muse::ContextInject<muse::ui::IMainWindow> mainWindow{ this };
    muse::ContextInject<record::IRecordController> recordController{ this };
    muse::ContextInject<context::IUiContextResolver> uiContextResolver{ this };

public:
    AppShellCommandsState(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    std::string moduleName() const override;

    void init() override;
    void deinit() override;

    muse::rcommand::CommandState commandState(const muse::rcommand::Command& command) const override;
    muse::async::Channel<muse::rcommand::Command, muse::rcommand::CommandState> commandStateChanged() const override;

private:
    void updateCommandStates(const std::vector<muse::rcommand::Command>& commands = {});

    muse::rcommand::IModuleCommandsRegisterPtr m_moduleRegister;
    std::map<muse::rcommand::Command, muse::rcommand::CommandState> m_commandStates;
    muse::async::Channel<muse::rcommand::Command, muse::rcommand::CommandState> m_commandStateChanged;
};
}
