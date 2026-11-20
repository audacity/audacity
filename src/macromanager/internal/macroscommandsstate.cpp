/*
* Audacity: A Digital Audio Editor
*/
#include "macroscommandsstate.h"

#include "framework/global/log.h"

using namespace au::macromanager;
using namespace muse;
using namespace muse::rcommand;

std::string MacrosCommandsState::moduleName() const
{
    return "macromanager";
}

void MacrosCommandsState::init()
{
    m_moduleRegister = commandsRegister()->moduleRegister(moduleName());
    IF_ASSERT_FAILED(m_moduleRegister) {
        return;
    }

    m_moduleRegister->commandListChanged().onNotify(this, [this]() {
        sendCommandStates();
    });

    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        sendCommandStates();
    });

    runner()->isRunningChanged().onNotify(this, [this]() {
        sendCommandStates();
    });
}

void MacrosCommandsState::deinit()
{
    if (m_moduleRegister) {
        m_moduleRegister->commandListChanged().disconnect(this);
    }
    globalContext()->currentProjectChanged().disconnect(this);
    runner()->isRunningChanged().disconnect(this);
}

bool MacrosCommandsState::canRunMacro() const
{
    return globalContext()->currentProject() != nullptr && !runner()->isRunning();
}

CommandState MacrosCommandsState::commandState(const Command&) const
{
    return CommandState(canRunMacro(), false);
}

void MacrosCommandsState::sendCommandStates()
{
    const CommandState state(canRunMacro(), false);
    for (const Command& command : m_moduleRegister->commandList()) {
        m_commandStateChanged.send(command, state);
    }
}

async::Channel<Command, CommandState> MacrosCommandsState::commandStateChanged() const
{
    return m_commandStateChanged;
}
