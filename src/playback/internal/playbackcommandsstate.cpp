/*
* Audacity: A Digital Audio Editor
*/
#include "playbackcommandsstate.h"

#include "framework/global/log.h"

#include "../playbackcommands.h"

using namespace au::playback;
using namespace muse;
using namespace muse::rcommand;

namespace {
const std::vector<Command> PLAYING_DEPENDENT_COMMANDS = {
    PLAYBACK_TOGGLE_PLAY_PAUSE_COMMAND,
    PLAYBACK_TOGGLE_PLAY_STOP_COMMAND,
    PLAYBACK_TOGGLE_PLAY_STOP_AND_SET_CURSOR_COMMAND,
    PLAYBACK_PLAY_SELECTION_COMMAND,
    PLAYBACK_PAUSE_COMMAND,
    PLAYBACK_REWIND_START_COMMAND,
    PLAYBACK_REWIND_END_COMMAND,
};
}

std::string PlaybackCommandsState::moduleName() const
{
    return "playback";
}

void PlaybackCommandsState::init()
{
    m_moduleRegister = commandsRegister()->moduleRegister(moduleName());
    IF_ASSERT_FAILED(m_moduleRegister) {
        return;
    }

    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        updateCommandStates();
    });

    controller()->isPlayAllowedChanged().onNotify(this, [this]() {
        updateCommandStates();
    });

    controller()->loopRegionChanged().onNotify(this, [this]() {
        updateCommandStates({ PLAYBACK_TOGGLE_LOOP_REGION_COMMAND });
    });

    controller()->isPlayingChanged().onNotify(this, [this]() {
        updateCommandStates(PLAYING_DEPENDENT_COMMANDS);
    });

    selectionController()->dataSelectedStartTimeChanged().onReceive(this, [this](trackedit::secs_t) {
        updateCommandStates({ PLAYBACK_PLAY_SELECTION_COMMAND });
    });

    selectionController()->dataSelectedEndTimeChanged().onReceive(this, [this](trackedit::secs_t) {
        updateCommandStates({ PLAYBACK_PLAY_SELECTION_COMMAND });
    });

    playbackConfiguration()->selectionFollowsLoopRegionChanged().onNotify(this, [this]() {
        updateCommandStates({ PLAYBACK_TOGGLE_SELECTION_FOLLOWS_LOOP_REGION_COMMAND });
    });

    updateCommandStates();
}

void PlaybackCommandsState::deinit()
{
    globalContext()->currentProjectChanged().disconnect(this);
    controller()->isPlayAllowedChanged().disconnect(this);
    controller()->isPlayingChanged().disconnect(this);
    selectionController()->dataSelectedStartTimeChanged().disconnect(this);
    selectionController()->dataSelectedEndTimeChanged().disconnect(this);
    playbackConfiguration()->selectionFollowsLoopRegionChanged().disconnect(this);
    controller()->loopRegionChanged().disconnect(this);
}

void PlaybackCommandsState::updateCommandStates(const std::vector<Command>& commands)
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

CommandState PlaybackCommandsState::commandState(const Command& command) const
{
    if (globalContext()->currentProject() == nullptr) {
        return CommandState(false, false);
    }

    const bool isRecording = recordController()->isRecording();

    if (command == PLAYBACK_TOGGLE_PLAY_STOP_COMMAND
        || command == PLAYBACK_TOGGLE_PLAY_STOP_AND_SET_CURSOR_COMMAND) {
        return CommandState(!isRecording, false);
    }

    if (command == PLAYBACK_PLAY_SELECTION_COMMAND) {
        return CommandState(!isRecording && (!controller()->isStopped() || !selectionController()->timeSelectionIsEmpty()), false);
    }

    if (command == PLAYBACK_REWIND_START_COMMAND || command == PLAYBACK_REWIND_END_COMMAND) {
        return CommandState(!controller()->isPlaying() && !isRecording, false);
    }

    if (command == PLAYBACK_TOGGLE_LOOP_REGION_COMMAND) {
        return CommandState(true, controller()->isLoopRegionActive());
    }

    if (command == PLAYBACK_TOGGLE_SELECTION_FOLLOWS_LOOP_REGION_COMMAND) {
        return CommandState(true, playbackConfiguration()->selectionFollowsLoopRegion());
    }

    return CommandState(true, false);
}

async::Channel<Command, CommandState> PlaybackCommandsState::commandStateChanged() const
{
    return m_commandStateChanged;
}
