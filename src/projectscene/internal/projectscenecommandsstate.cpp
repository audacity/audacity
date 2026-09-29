/*
* Audacity: A Digital Audio Editor
*/
#include "projectscenecommandsstate.h"

#include "framework/global/log.h"

#include "../projectscenecommands.h"

using namespace au::projectscene;
using namespace muse;
using namespace muse::rcommand;

std::string ProjectSceneCommandsState::moduleName() const
{
    return "projectscene";
}

void ProjectSceneCommandsState::init()
{
    m_moduleRegister = commandsRegister()->moduleRegister(moduleName());
    IF_ASSERT_FAILED(m_moduleRegister) {
        return;
    }

    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        updateCommandStates();
    });

    projectSceneUiState()->timelineRulerModeChanged().onNotify(this, [this]() {
        updateCommandStates({ PROJECTSCENE_MINUTES_SECONDS_RULER_COMMAND, PROJECTSCENE_BEATS_MEASURES_RULER_COMMAND });
    });

    configuration()->isVerticalRulersVisibleChanged().onReceive(this, [this](bool) {
        updateCommandStates({ PROJECTSCENE_TOGGLE_VERTICAL_RULERS_COMMAND });
    });

    configuration()->isRMSInWaveformVisibleChanged().onReceive(this, [this](bool) {
        updateCommandStates({ PROJECTSCENE_TOGGLE_RMS_IN_WAVEFORM_COMMAND });
    });

    configuration()->isClippingInWaveformVisibleChanged().onReceive(this, [this](bool) {
        updateCommandStates({ PROJECTSCENE_TOGGLE_CLIPPING_IN_WAVEFORM_COMMAND });
    });

    configuration()->updateDisplayWhilePlayingEnabledChanged().onNotify(this, [this]() {
        updateCommandStates({ PROJECTSCENE_TOGGLE_UPDATE_DISPLAY_WHILE_PLAYING_COMMAND });
    });

    configuration()->pinnedPlayHeadEnabledChanged().onNotify(this, [this]() {
        updateCommandStates({ PROJECTSCENE_TOGGLE_PINNED_PLAY_HEAD_COMMAND });
    });

    configuration()->playbackOnRulerClickEnabledChanged().onNotify(this, [this]() {
        updateCommandStates({ PROJECTSCENE_TOGGLE_PLAYBACK_ON_RULER_CLICK_COMMAND });
    });

    updateCommandStates();
}

void ProjectSceneCommandsState::deinit()
{
    globalContext()->currentProjectChanged().disconnect(this);
    projectSceneUiState()->timelineRulerModeChanged().disconnect(this);
    configuration()->isVerticalRulersVisibleChanged().disconnect(this);
    configuration()->isRMSInWaveformVisibleChanged().disconnect(this);
    configuration()->isClippingInWaveformVisibleChanged().disconnect(this);
    configuration()->updateDisplayWhilePlayingEnabledChanged().disconnect(this);
    configuration()->pinnedPlayHeadEnabledChanged().disconnect(this);
    configuration()->playbackOnRulerClickEnabledChanged().disconnect(this);
}

void ProjectSceneCommandsState::updateCommandStates(const std::vector<Command>& commands)
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

CommandState ProjectSceneCommandsState::commandState(const Command& command) const
{
    if (globalContext()->currentProject() == nullptr) {
        return CommandState(false, false);
    }

    if (command == PROJECTSCENE_MINUTES_SECONDS_RULER_COMMAND) {
        return CommandState(true, projectSceneUiState()->timelineRulerMode() == TimelineRulerMode::MINUTES_AND_SECONDS);
    }

    if (command == PROJECTSCENE_BEATS_MEASURES_RULER_COMMAND) {
        return CommandState(true, projectSceneUiState()->timelineRulerMode() == TimelineRulerMode::BEATS_AND_MEASURES);
    }

    if (command == PROJECTSCENE_TOGGLE_VERTICAL_RULERS_COMMAND) {
        return CommandState(true, configuration()->isVerticalRulersVisible());
    }

    if (command == PROJECTSCENE_TOGGLE_RMS_IN_WAVEFORM_COMMAND) {
        return CommandState(true, configuration()->isRMSInWaveformVisible());
    }

    if (command == PROJECTSCENE_TOGGLE_CLIPPING_IN_WAVEFORM_COMMAND) {
        return CommandState(true, configuration()->isClippingInWaveformVisible());
    }

    if (command == PROJECTSCENE_TOGGLE_UPDATE_DISPLAY_WHILE_PLAYING_COMMAND) {
        return CommandState(true, configuration()->updateDisplayWhilePlayingEnabled());
    }

    if (command == PROJECTSCENE_TOGGLE_PINNED_PLAY_HEAD_COMMAND) {
        return CommandState(true, configuration()->pinnedPlayHeadEnabled());
    }

    if (command == PROJECTSCENE_TOGGLE_PLAYBACK_ON_RULER_CLICK_COMMAND) {
        return CommandState(true, configuration()->playbackOnRulerClickEnabled());
    }

    return CommandState(true, false);
}

async::Channel<Command, CommandState> ProjectSceneCommandsState::commandStateChanged() const
{
    return m_commandStateChanged;
}
