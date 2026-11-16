/*
* Audacity: A Digital Audio Editor
*/
#include "appshellcommandsstate.h"

#include "framework/global/log.h"
#include "framework/dockwindow/idockwindow.h"

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
        updateCommandStates({ APP_QUIT_COMMAND, APP_RESTART_COMMAND });
    });

    mainWindow()->isFullScreenChanged().onNotify(this, [this]() {
        updateCommandStates({ APP_TOGGLE_FULLSCREEN_COMMAND });
    });

    uiContextResolver()->currentUiContextChanged().onNotify(this, [this]() {
        updateCommandStates({ APP_CONTEXT_MENU_COMMAND });
    });

    dockWindowProvider()->windowChanged().onNotify(this, [this]() {
        listenOpenedDocksChanged(dockWindowProvider()->window());

        std::vector<Command> dockCommands;
        for (const auto& [command, dockName] : toggleDockCommands()) {
            dockCommands.push_back(command);
        }
        updateCommandStates(dockCommands);
    });
    listenOpenedDocksChanged(dockWindowProvider()->window());

    updateCommandStates();
}

void AppShellCommandsState::deinit()
{
    recordController()->isRecordingChanged().disconnect(this);
    mainWindow()->isFullScreenChanged().disconnect(this);
    uiContextResolver()->currentUiContextChanged().disconnect(this);
    dockWindowProvider()->windowChanged().disconnect(this);
    if (muse::dock::IDockWindow* window = dockWindowProvider()->window()) {
        window->docksOpenStatusChanged().disconnect(this);
    }
}

void AppShellCommandsState::listenOpenedDocksChanged(muse::dock::IDockWindow* window)
{
    if (!window) {
        return;
    }

    window->docksOpenStatusChanged().onReceive(this, [this](const QStringList& dockNames) {
        std::vector<Command> commands;
        for (const auto& [command, dockName] : toggleDockCommands()) {
            if (dockNames.contains(dockName)) {
                commands.push_back(command);
            }
        }

        if (!commands.empty()) {
            updateCommandStates(commands);
        }
    });
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
    if (command == APP_QUIT_COMMAND || command == APP_RESTART_COMMAND) {
        return CommandState(!recordController()->isRecording(), false);
    }

    if (command == APP_TOGGLE_FULLSCREEN_COMMAND) {
        return CommandState(true, mainWindow()->isFullScreen());
    }

    if (command == APP_CONTEXT_MENU_COMMAND) {
        return CommandState(uiContextResolver()->matchWithCurrent(context::UiCtxProjectFocused), false);
    }

    const auto& dockCommands = toggleDockCommands();
    if (const auto it = dockCommands.find(command); it != dockCommands.end()) {
        const muse::dock::IDockWindow* window = dockWindowProvider()->window();
        return CommandState(true, window ? window->isDockOpen(it->second) : false);
    }

    return CommandState(true, false);
}

async::Channel<Command, CommandState> AppShellCommandsState::commandStateChanged() const
{
    return m_commandStateChanged;
}
