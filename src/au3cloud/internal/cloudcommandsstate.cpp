/*
* Audacity: A Digital Audio Editor
*/
#include "cloudcommandsstate.h"

#include "framework/global/log.h"

using namespace au::au3cloud;
using namespace muse::rcommand;

std::string CloudCommandsState::moduleName() const
{
    return "au3cloud";
}

void CloudCommandsState::init()
{
    m_moduleRegister = commandsRegister()->moduleRegister(moduleName());
    IF_ASSERT_FAILED(m_moduleRegister) {
        return;
    }

    updateCommandStates();
}

void CloudCommandsState::deinit()
{
}

void CloudCommandsState::updateCommandStates(const std::vector<Command>& commands)
{
    IF_ASSERT_FAILED(m_moduleRegister) {
        return;
    }

    const auto& commandList = commands.empty() ? m_moduleRegister->commandList() : commands;

    for (const Command& command : commandList) {
        const CommandState newState = commandState(command);
        if (m_commandStates[command] != newState) {
            m_commandStates[command] = newState;
            m_commandStateChanged.send(command, newState);
        }
    }
}

CommandState CloudCommandsState::commandState(const Command&) const
{
    return CommandState(true, false);
}

muse::async::Channel<Command, CommandState> CloudCommandsState::commandStateChanged() const
{
    return m_commandStateChanged;
}
