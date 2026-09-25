/*
* Audacity: A Digital Audio Editor
*/
#include "appshellcommandsstate.h"

#include "framework/global/log.h"

#include "../appshellcommands.h"

using namespace au::appshell;
using namespace muse;
using namespace muse::rcommand;

std::string AppShellCommandsState::moduleName() const
{
    return "appshell";
}

void AppShellCommandsState::init()
{
    m_moduleRegister = commandsRegister()->moduleRegister(moduleName());
    IF_ASSERT_FAILED(m_moduleRegister) {
        return;
    }

    recordController()->isRecordingChanged().onNotify(this, [this]() {
        updateCommandStates({ GLOBAL_QUIT_COMMAND, GLOBAL_RESTART_COMMAND });
    });

    mainWindow()->isFullScreenChanged().onNotify(this, [this]() {
        updateCommandStates({ APP_TOGGLE_FULLSCREEN_COMMAND });
    });

    uiContextResolver()->currentUiContextChanged().onNotify(this, [this]() {
        updateCommandStates({ GLOBAL_CONTEXT_MENU_COMMAND });
    });

    updateCommandStates();
}

void AppShellCommandsState::deinit()
{
    recordController()->isRecordingChanged().disconnect(this);
    mainWindow()->isFullScreenChanged().disconnect(this);
    uiContextResolver()->currentUiContextChanged().disconnect(this);
}

void AppShellCommandsState::updateCommandStates(const std::vector<Command>& commands)
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

CommandState AppShellCommandsState::commandState(const Command& command) const
{
    if (command == GLOBAL_QUIT_COMMAND || command == GLOBAL_RESTART_COMMAND) {
        return CommandState(!recordController()->isRecording(), false);
    }

    if (command == APP_TOGGLE_FULLSCREEN_COMMAND) {
        return CommandState(true, mainWindow()->isFullScreen());
    }

    if (command == GLOBAL_CONTEXT_MENU_COMMAND) {
        return CommandState(uiContextResolver()->matchWithCurrent(context::UiCtxProjectFocused), false);
    }

    return CommandState(true, false);
}

async::Channel<Command, CommandState> AppShellCommandsState::commandStateChanged() const
{
    return m_commandStateChanged;
}
