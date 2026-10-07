/*
* Audacity: A Digital Audio Editor
*/
#include "trackeditcommandsstate.h"

#include <algorithm>
#include <limits>

#include "framework/global/containers.h"
#include "framework/global/log.h"

#include "../trackeditcommands.h"

using namespace au::trackedit;
using namespace muse;
using namespace muse::rcommand;

namespace {
const std::vector<Command> commandsDisabledDuringRecording {
    TRACKEDIT_UNDO_COMMAND,
    TRACKEDIT_REDO_COMMAND,
    TRACKEDIT_PASTE_OVERLAP_COMMAND,
    TRACKEDIT_PASTE_INSERT_COMMAND,
    TRACKEDIT_PASTE_INSERT_ALL_TRACKS_RIPPLE_COMMAND,
    TRACKEDIT_CLIP_CUT_COMMAND,
    TRACKEDIT_CLIP_DELETE_COMMAND,
    TRACKEDIT_CLIP_SPLIT_CUT_COMMAND,
    TRACKEDIT_CLIP_SPLIT_DELETE_COMMAND,
    TRACKEDIT_CLIP_RENDER_PITCH_SPEED_COMMAND,
    TRACKEDIT_CLIP_RESET_PITCH_SPEED_COMMAND,
    TRACKEDIT_STRETCH_CLIP_TO_MATCH_TEMPO_COMMAND,
    TRACKEDIT_TRACK_SPLIT_AT_COMMAND,
    TRACKEDIT_NEW_MONO_TRACK_COMMAND,
    TRACKEDIT_NEW_STEREO_TRACK_COMMAND,
    TRACKEDIT_NEW_LABEL_TRACK_COMMAND,
    TRACKEDIT_TRACK_DUPLICATE_COMMAND,
    TRACKEDIT_TRACK_SWAP_CHANNELS_COMMAND,
    TRACKEDIT_TRACK_SPLIT_STEREO_TO_LR_COMMAND,
    TRACKEDIT_TRACK_SPLIT_STEREO_TO_CENTER_COMMAND,
    TRACKEDIT_TRACK_CHANGE_RATE_CUSTOM_COMMAND,
    TRACKEDIT_TRACK_MAKE_STEREO_COMMAND,
    TRACKEDIT_TRACK_RESAMPLE_COMMAND,
    TRACKEDIT_TRIM_AUDIO_OUTSIDE_SELECTION_COMMAND,
    TRACKEDIT_SILENCE_AUDIO_SELECTION_COMMAND,
    TRACKEDIT_GROUP_CLIPS_COMMAND,
    TRACKEDIT_UNGROUP_CLIPS_COMMAND,
    TRACKEDIT_MERGE_SELECTED_ON_TRACKS_COMMAND,
    TRACKEDIT_DUPLICATE_SELECTED_COMMAND,
    TRACKEDIT_TRACK_DELETE_COMMAND,
    TRACKEDIT_CUT_COMMAND,
    TRACKEDIT_CUT_LEAVE_GAP_COMMAND,
    TRACKEDIT_CUT_PER_CLIP_RIPPLE_COMMAND,
    TRACKEDIT_CUT_PER_TRACK_RIPPLE_COMMAND,
    TRACKEDIT_CUT_ALL_TRACKS_RIPPLE_COMMAND,
    TRACKEDIT_DELETE_COMMAND,
    TRACKEDIT_DELETE_LEAVE_GAP_COMMAND,
    TRACKEDIT_DELETE_PER_CLIP_RIPPLE_COMMAND,
    TRACKEDIT_DELETE_PER_TRACK_RIPPLE_COMMAND,
    TRACKEDIT_DELETE_ALL_TRACKS_RIPPLE_COMMAND,
    TRACKEDIT_PASTE_DEFAULT_COMMAND,
    TRACKEDIT_SPLIT_COMMAND,
    TRACKEDIT_SPLIT_INTO_NEW_TRACK_COMMAND,
    TRACKEDIT_JOIN_COMMAND,
    TRACKEDIT_DUPLICATE_COMMAND,
    TRACKEDIT_TRACK_SPLIT_COMMAND,
};

const std::vector<Command> historyCommands {
    TRACKEDIT_UNDO_COMMAND,
    TRACKEDIT_REDO_COMMAND,
    TRACKEDIT_SILENCE_AUDIO_SELECTION_COMMAND,
};

const std::vector<Command> clipsSelectionCommands {
    TRACKEDIT_GROUP_CLIPS_COMMAND,
    TRACKEDIT_UNGROUP_CLIPS_COMMAND,
    TRACKEDIT_JOIN_COMMAND,
    TRACKEDIT_SILENCE_AUDIO_SELECTION_COMMAND,
    TRACKEDIT_RENAME_ITEM_COMMAND,
};

const std::vector<Command> rangeSelectionCommands {
    TRACKEDIT_JOIN_COMMAND,
};

const std::vector<Command> labelsSelectionCommands {
    TRACKEDIT_RENAME_ITEM_COMMAND,
};

const std::vector<Command> dataSelectionCommands {
    TRACKEDIT_SILENCE_AUDIO_SELECTION_COMMAND,
};
}

std::string TrackeditCommandsState::moduleName() const
{
    return "trackedit";
}

void TrackeditCommandsState::init()
{
    m_moduleRegister = commandsRegister()->moduleRegister(moduleName());
    IF_ASSERT_FAILED(m_moduleRegister) {
        return;
    }

    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        updateCommandStates();
    });

    globalContext()->isRecordingChanged().onNotify(this, [this]() {
        updateCommandStates(commandsDisabledDuringRecording);
    });

    projectHistory()->historyChanged().onReceive(this, [this](auto) {
        updateCommandStates(historyCommands);
    });

    selectionController()->clipsSelected().onReceive(this, [this](const ClipKeyList&) {
        updateCommandStates(clipsSelectionCommands);
    });

    selectionController()->labelsSelected().onReceive(this, [this](const LabelKeyList&) {
        updateCommandStates(labelsSelectionCommands);
    });

    selectionController()->clipsIntersectingRangeSelectionChanged().onReceive(this, [this](const ClipKeyList&) {
        updateCommandStates(rangeSelectionCommands);
    });

    selectionController()->selectedTracksChanged().onReceive(this, [this](const TrackIdList&) {
        updateCommandStates(dataSelectionCommands);
    });

    selectionController()->dataSelectedStartTimeChanged().onReceive(this, [this](secs_t) {
        updateCommandStates(dataSelectionCommands);
    });

    selectionController()->dataSelectedEndTimeChanged().onReceive(this, [this](secs_t) {
        updateCommandStates(dataSelectionCommands);
    });

    updateCommandStates();
}

void TrackeditCommandsState::deinit()
{
    globalContext()->currentProjectChanged().disconnect(this);
    globalContext()->isRecordingChanged().disconnect(this);
    projectHistory()->historyChanged().disconnect(this);
    selectionController()->clipsSelected().disconnect(this);
    selectionController()->labelsSelected().disconnect(this);
    selectionController()->clipsIntersectingRangeSelectionChanged().disconnect(this);
    selectionController()->selectedTracksChanged().disconnect(this);
    selectionController()->dataSelectedStartTimeChanged().disconnect(this);
    selectionController()->dataSelectedEndTimeChanged().disconnect(this);
}

void TrackeditCommandsState::updateCommandStates(const std::vector<Command>& commands)
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

CommandState TrackeditCommandsState::commandState(const Command& command) const
{
    if (globalContext()->currentProject() == nullptr) {
        return CommandState(false);
    }

    if (globalContext()->isRecording() && muse::contains(commandsDisabledDuringRecording, command)) {
        return CommandState(false);
    }

    if (command == TRACKEDIT_UNDO_COMMAND) {
        return CommandState(projectHistory()->undoAvailable());
    }

    if (command == TRACKEDIT_REDO_COMMAND) {
        return CommandState(projectHistory()->redoAvailable());
    }

    if (command == TRACKEDIT_GROUP_CLIPS_COMMAND) {
        return CommandState(clipsForInteraction().size() > 1 && !selectionController()->isSelectionGrouped());
    }

    if (command == TRACKEDIT_UNGROUP_CLIPS_COMMAND) {
        return CommandState(clipsForInteraction().size() > 1 && selectionController()->selectionContainsGroup());
    }

    if (command == TRACKEDIT_JOIN_COMMAND) {
        return CommandState(canJoin());
    }

    if (command == TRACKEDIT_SILENCE_AUDIO_SELECTION_COMMAND) {
        return CommandState(canSilenceAudio());
    }

    if (command == TRACKEDIT_RENAME_ITEM_COMMAND) {
        return CommandState(clipsForInteraction().size() + labelsForInteraction().size() == 1);
    }

    return CommandState(true);
}

ClipKeyList TrackeditCommandsState::clipsForInteraction() const
{
    ClipKeyList result = selectionController()->selectedClips();

    const std::optional<TrackItemKey> focusedItemKey = trackNavigationController()->focus().itemKey();
    if (!focusedItemKey || muse::contains(result, *focusedItemKey)) {
        return result;
    }

    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (prj && prj->track(focusedItemKey->trackId)->type != TrackType::Label) {
        result.insert(result.cbegin(), *focusedItemKey);
    }

    return result;
}

LabelKeyList TrackeditCommandsState::labelsForInteraction() const
{
    LabelKeyList result = selectionController()->selectedLabels();

    const std::optional<TrackItemKey> focusedItemKey = trackNavigationController()->focus().itemKey();
    if (!focusedItemKey || muse::contains(result, *focusedItemKey)) {
        return result;
    }

    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (prj && prj->track(focusedItemKey->trackId)->type == TrackType::Label) {
        result.insert(result.cbegin(), *focusedItemKey);
    }

    return result;
}

bool TrackeditCommandsState::canJoin() const
{
    if (!selectionController()->timeSelectionIsEmpty()) {
        ClipKeyList clips = selectionController()->clipsIntersectingRangeSelection();
        std::sort(clips.begin(), clips.end());

        for (size_t i = 1; i < clips.size(); ++i) {
            if (clips[i].trackId == clips[i - 1].trackId) {
                return true;
            }
        }

        return false;
    }

    const ClipKeyList clipKeys = selectionController()->selectedClips();
    if (clipKeys.size() < 2) {
        return false;
    }

    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return false;
    }

    const TrackId trackId = clipKeys.front().trackId;
    for (const ClipKey& key : clipKeys) {
        if (key.trackId != trackId) {
            return false;
        }
    }

    const muse::async::NotifyList<Clip> trackClips = prj->clipList(trackId);

    size_t selectedCount = 0;
    double begin = std::numeric_limits<double>::max();
    double end = std::numeric_limits<double>::lowest();
    for (const Clip& clip : trackClips) {
        if (muse::contains(clipKeys, clip.key)) {
            ++selectedCount;
            begin = std::min(begin, clip.startTime);
            end = std::max(end, clip.endTime);
        }
    }

    if (selectedCount != clipKeys.size()) {
        return false;
    }

    for (const Clip& clip : trackClips) {
        if (!muse::contains(clipKeys, clip.key) && clip.startTime < end && clip.endTime > begin) {
            return false;
        }
    }

    return true;
}

bool TrackeditCommandsState::canSilenceAudio() const
{
    if (!selectionController()->timeSelectionIsEmpty()) {
        return !trackeditInteraction()->tracksDataIsSilent(selectionController()->selectedTracks(),
                                                           selectionController()->dataSelectedStartTime(),
                                                           selectionController()->dataSelectedEndTime());
    }

    for (const auto& clipKey : selectionController()->selectedClips()) {
        const secs_t begin = trackeditInteraction()->clipStartTime(clipKey);
        const secs_t end = trackeditInteraction()->clipEndTime(clipKey);
        if (!trackeditInteraction()->tracksDataIsSilent({ clipKey.trackId }, begin, end)) {
            return true;
        }
    }

    return false;
}

async::Channel<Command, CommandState> TrackeditCommandsState::commandStateChanged() const
{
    return m_commandStateChanged;
}
