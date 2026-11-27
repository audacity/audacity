/*
* Audacity: A Digital Audio Editor
*/
#include "projectscenecommandsstate.h"

#include <algorithm>
#include <functional>

#include "framework/global/containers.h"
#include "framework/global/log.h"

#include "project/iaudacityproject.h"

#include "../projectscenecommands.h"

using namespace au::projectscene;
using namespace muse;
using namespace muse::rcommand;

namespace {
const std::vector<Command> TRACK_HEIGHT_COMMANDS {
    PROJECTSCENE_INCREASE_TRACK_HEIGHT_COMMAND,
    PROJECTSCENE_DECREASE_TRACK_HEIGHT_COMMAND,
    PROJECTSCENE_INCREASE_ALL_TRACK_HEIGHTS_COMMAND,
    PROJECTSCENE_DECREASE_ALL_TRACK_HEIGHTS_COMMAND,
    PROJECTSCENE_COLLAPSE_TRACKS_COMMAND,
    PROJECTSCENE_EXPAND_TRACKS_COMMAND,
    PROJECTSCENE_COLLAPSE_ALL_TRACKS_COMMAND,
    PROJECTSCENE_EXPAND_ALL_TRACKS_COMMAND,
    PROJECTSCENE_FIT_TRACKS_TO_HEIGHT_COMMAND,
    PROJECTSCENE_FIT_ALL_TRACKS_TO_HEIGHT_COMMAND,
    PROJECTSCENE_RESET_TRACK_HEIGHT_COMMAND,
    PROJECTSCENE_RESET_ALL_TRACK_HEIGHTS_COMMAND,
};

bool anyTrack(const au::context::IGlobalContext& globalContext,
              const std::function<bool(const IProjectViewState&, au::trackedit::TrackId)>& predicate)
{
    const au::project::IAudacityProjectPtr project = globalContext.currentProject();
    const IProjectViewStatePtr viewState = project ? project->viewState() : nullptr;
    const au::trackedit::ITrackeditProjectPtr trackeditProject = globalContext.currentTrackeditProject();
    if (!viewState || !trackeditProject) {
        return false;
    }

    const au::trackedit::TrackIdList tracks = trackeditProject->trackIdList();
    return std::any_of(tracks.begin(), tracks.end(), [&viewState, &predicate](const au::trackedit::TrackId trackId) {
        return predicate(*viewState, trackId);
    });
}
}

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
        subscribeToTrackHeights();
        updateCommandStates();
    });
    subscribeToTrackHeights();

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

    configuration()->isEffectsPanelVisibleChanged().onNotify(this, [this]() {
        updateCommandStates({ PROJECTSCENE_TOGGLE_EFFECTS_PANEL_COMMAND });
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
    configuration()->isEffectsPanelVisibleChanged().disconnect(this);
}

void ProjectSceneCommandsState::subscribeToTrackHeights()
{
    const project::IAudacityProjectPtr project = globalContext()->currentProject();
    const IProjectViewStatePtr viewState = project ? project->viewState() : nullptr;
    if (!viewState) {
        return;
    }

    // Collapsing, expanding, adding or removing a track changes the total height
    viewState->totalTrackHeight().ch.onReceive(this, [this](int) {
        updateCommandStates(TRACK_HEIGHT_COMMANDS);
    }, muse::async::Asyncable::Mode::SetReplace);
}

bool ProjectSceneCommandsState::anyTrackCollapsed() const
{
    return anyTrack(*globalContext(), [](const IProjectViewState& viewState, trackedit::TrackId trackId) {
        return viewState.isTrackCollapsed(trackId).val;
    });
}

bool ProjectSceneCommandsState::anyTrackExpanded() const
{
    return anyTrack(*globalContext(), [](const IProjectViewState& viewState, trackedit::TrackId trackId) {
        return !viewState.isTrackCollapsed(trackId).val;
    });
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

    if (command == PROJECTSCENE_TOGGLE_EFFECTS_PANEL_COMMAND) {
        return CommandState(true, configuration()->isEffectsPanelVisible());
    }

    if (command == PROJECTSCENE_COLLAPSE_ALL_TRACKS_COMMAND) {
        return CommandState(anyTrackExpanded(), false);
    }

    if (command == PROJECTSCENE_EXPAND_ALL_TRACKS_COMMAND) {
        return CommandState(anyTrackCollapsed(), false);
    }

    if (muse::contains(TRACK_HEIGHT_COMMANDS, command)) {
        const trackedit::ITrackeditProjectPtr trackeditProject = globalContext()->currentTrackeditProject();
        return CommandState(trackeditProject && !trackeditProject->trackIdList().empty(), false);
    }

    return CommandState(true, false);
}

async::Channel<Command, CommandState> ProjectSceneCommandsState::commandStateChanged() const
{
    return m_commandStateChanged;
}
