/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramcommandsstate.h"

#include "framework/global/log.h"

namespace au::spectrogram {
using muse::rcommand::Command;
using muse::rcommand::CommandState;

std::string SpectrogramCommandsState::moduleName() const
{
    return "spectrogram";
}

void SpectrogramCommandsState::init()
{
    m_moduleRegister = commandsRegister()->moduleRegister(moduleName());
    IF_ASSERT_FAILED(m_moduleRegister) {
        return;
    }

    updateCommandStates();
}

void SpectrogramCommandsState::deinit()
{
}

void SpectrogramCommandsState::updateCommandStates(const std::vector<Command>& commands)
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

CommandState SpectrogramCommandsState::commandState(const Command&) const
{
    return CommandState(true, false);
}

muse::async::Channel<Command, CommandState> SpectrogramCommandsState::commandStateChanged() const
{
    return m_commandStateChanged;
}
}
