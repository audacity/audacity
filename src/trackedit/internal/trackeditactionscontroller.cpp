/*
* Audacity: A Digital Audio Editor
*/
#include "trackeditactionscontroller.h"
#include "project/internal/audacityproject.h"
#include "trackediterrors.h"
#include "translation.h"

using namespace muse;
using namespace au::trackedit;
using namespace muse::async;
using namespace muse::actions;

static const ActionCode COPY_CODE("copy");
static const ActionCode CUT_CODE("cut");
static const ActionCode PASTE_CODE("paste");
static const ActionCode DELETE_CODE("delete");
static const ActionCode SPLIT_CODE("split");
static const ActionCode SPLIT_INTO_NEW_TRACK_CODE("split-into-new-track");
static const ActionCode JOIN_CODE("join");
static const ActionCode DISJOIN_CODE("disjoin");
static const ActionCode DUPLICATE_CODE("duplicate");

static const ActionCode CUT_PER_CLIP_RIPPLE_CODE("cut-per-clip-ripple");
static const ActionCode CUT_PER_TRACK_RIPPLE_CODE("cut-per-track-ripple");
static const ActionCode CUT_ALL_TRACKS_RIPPLE_CODE("cut-all-tracks-ripple");

static const ActionCode DELETE_LEAVE_GAP_CODE("split-delete");
static const ActionCode DELETE_PER_CLIP_RIPPLE_CODE("delete-per-clip-ripple");
static const ActionCode DELETE_PER_TRACK_RIPPLE_CODE("delete-per-track-ripple");
static const ActionCode DELETE_ALL_TRACKS_RIPPLE_CODE("delete-all-tracks-ripple");

static const ActionCode CLIP_CUT_CODE("clip-cut");
static const ActionCode MULTI_CLIP_CUT_CODE("multi-clip-cut");
static const ActionCode RANGE_SELECTION_CUT_CODE("clip-cut-selected");

static const ActionCode CLIP_COPY_CODE("clip-copy");
static const ActionCode MULTI_CLIP_COPY_CODE("multi-clip-copy");
static const ActionCode RANGE_SELECTION_COPY_CODE("clip-copy-selected");

static const ActionCode PASTE_INSERT_CODE("paste-insert");
static const ActionCode PASTE_INSERT_ALL_TRACKS_RIPPLE_CODE("paste-insert-all-tracks-ripple");

static const ActionCode CLIP_DELETE_CODE("clip-delete");
static const ActionCode MULTI_CLIP_DELETE_CODE("multi-clip-delete");
static const ActionCode RANGE_SELECTION_DELETE_CODE("clip-delete-selected");

static const ActionCode OPEN_CLIP_AND_SPEED_CODE("clip-pitch-speed-open");
static const ActionCode CLIP_RENDER_PITCH_AND_SPEED_CODE("clip-render-pitch-speed");
static const ActionCode TRACK_SPLIT("track-split");
static const ActionCode TRACK_SPLIT_AT("track-split-at");
static const ActionCode SPLIT_CLIPS_AT_SILENCES("split-clips-at-silences");
static const ActionCode SPLIT_RANGE_SELECTION_AT_SILENCES("split-range-selection-at-silences");
static const ActionCode SPLIT_RANGE_SELECTION_INTO_NEW_TRACKS("split-range-selection-into-new-tracks");
static const ActionCode SPLIT_CLIPS_INTO_NEW_TRACKS("split-clips-into-new-tracks");
static const ActionCode MERGE_SELECTED_ON_TRACK("merge-selected-on-tracks");
static const ActionCode DUPLICATE_RANGE_SELECTION_CODE("duplicate-selected");
static const ActionCode DUPLICATE_CLIPS_CODE("duplicate-clips");
static const ActionCode CLIP_SPLIT_CUT("clip-split-cut");
static const ActionCode CLIP_SPLIT_DELETE("clip-split-delete");
static const ActionCode RANGE_SELECTION_SPLIT_CUT("split-cut-selected");
static const ActionCode RANGE_SELECTION_SPLIT_DELETE("split-delete-selected");
static const ActionCode NEW_MONO_TRACK("new-mono-track");
static const ActionCode NEW_STEREO_TRACK("new-stereo-track");
static const ActionCode NEW_LABEL_TRACK("new-label-track");
static const ActionCode TRACK_DELETE("track-delete");
static const ActionCode TRACK_DUPLICATE_CODE("track-duplicate");
static const ActionCode TRACK_MOVE_UP("track-move-up");
static const ActionCode TRACK_MOVE_DOWN("track-move-down");
static const ActionCode TRACK_MOVE_TOP("track-move-top");
static const ActionCode TRACK_MOVE_BOTTOM("track-move-bottom");
static const ActionCode TRACK_SWAP_CHANNELS("track-swap-channels");
static const ActionCode TRACK_SPLIT_STEREO_TO_LR("track-split-stereo-to-lr");
static const ActionCode TRACK_SPLIT_STEREO_TO_CENTER("track-split-stereo-to-center");
static const ActionCode TRACK_CHANGE_RATE_CUSTOM("track-change-rate-custom");
static const ActionCode TRACK_MAKE_STEREO("track-make-stereo");
static const ActionCode TRACK_RESAMPLE("track-resample");

static const ActionCode TRIM_AUDIO_OUTSIDE_SELECTION("trim-audio-outside-selection");
static const ActionCode SILENCE_AUDIO_SELECTION("silence-audio-selection");

static const ActionCode UNDO("undo");
static const ActionCode REDO("redo");

static const ActionCode STRETCH_ENABLED_CODE("stretch-clip-to-match-tempo");

static const ActionCode GROUP_CLIPS_CODE("group-clips");
static const ActionCode UNGROUP_CLIPS_CODE("ungroup-clips");

static const ActionQuery AUTO_COLOR_QUERY("action://trackedit/clip/change-color-auto");
static const ActionQuery CHANGE_COLOR_QUERY("action://trackedit/clip/change-color");
static const ActionQuery TRACK_CHANGE_COLOR_QUERY("action://trackedit/track/change-color");
static const ActionQuery TRACK_CHANGE_FORMAT_QUERY("action://trackedit/track/change-format");
static const ActionQuery TRACK_CHANGE_RATE_QUERY("action://trackedit/track/change-rate");

// In principle, disabled are actions that modify the data involved in playback.
static const std::vector<ActionCode> actionsDisabledDuringRecording {
    CUT_CODE,
    CUT_PER_CLIP_RIPPLE_CODE,
    CUT_PER_TRACK_RIPPLE_CODE,
    DELETE_CODE,
    DELETE_PER_CLIP_RIPPLE_CODE,
    DELETE_PER_TRACK_RIPPLE_CODE,
    DELETE_ALL_TRACKS_RIPPLE_CODE,
    SPLIT_CODE,
    SPLIT_INTO_NEW_TRACK_CODE,
    JOIN_CODE,
    DUPLICATE_CODE,
    CLIP_CUT_CODE,
    MULTI_CLIP_CUT_CODE,
    RANGE_SELECTION_CUT_CODE,
    CLIP_DELETE_CODE,
    MULTI_CLIP_DELETE_CODE,
    RANGE_SELECTION_DELETE_CODE,
    CLIP_RENDER_PITCH_AND_SPEED_CODE,
    PASTE_CODE,
    PASTE_INSERT_CODE,
    PASTE_INSERT_ALL_TRACKS_RIPPLE_CODE,
    TRACK_SPLIT,
    TRACK_SPLIT_AT,
    SPLIT_CLIPS_AT_SILENCES,
    SPLIT_RANGE_SELECTION_AT_SILENCES,
    SPLIT_RANGE_SELECTION_INTO_NEW_TRACKS,
    SPLIT_CLIPS_INTO_NEW_TRACKS,
    MERGE_SELECTED_ON_TRACK,
    DUPLICATE_RANGE_SELECTION_CODE,
    DUPLICATE_CLIPS_CODE,
    CLIP_SPLIT_CUT,
    CLIP_SPLIT_DELETE,
    RANGE_SELECTION_SPLIT_CUT,
    RANGE_SELECTION_SPLIT_DELETE,
    NEW_MONO_TRACK,
    NEW_STEREO_TRACK,
    NEW_LABEL_TRACK,
    TRIM_AUDIO_OUTSIDE_SELECTION,
    SILENCE_AUDIO_SELECTION,
    UNDO,
    REDO,
    STRETCH_ENABLED_CODE,
    TRACK_DELETE,
    TRACK_DUPLICATE_CODE,
    TRACK_SWAP_CHANNELS,
    TRACK_SPLIT_STEREO_TO_LR,
    TRACK_SPLIT_STEREO_TO_CENTER,
    TRACK_CHANGE_RATE_CUSTOM,
    TRACK_MAKE_STEREO,
    TRACK_RESAMPLE,
    GROUP_CLIPS_CODE,
    UNGROUP_CLIPS_CODE,
};

void TrackeditActionsController::init()
{
    dispatcher()->reg(this, COPY_CODE, this, &TrackeditActionsController::doGlobalCopy);
    dispatcher()->reg(this, CUT_CODE, this, &TrackeditActionsController::doGlobalCut);
    dispatcher()->reg(this, DELETE_CODE, this, &TrackeditActionsController::doGlobalDelete);
    dispatcher()->reg(this, SPLIT_CODE, this, &TrackeditActionsController::doGlobalSplit);
    dispatcher()->reg(this, SPLIT_INTO_NEW_TRACK_CODE, this, &TrackeditActionsController::doGlobalSplitIntoNewTrack);
    dispatcher()->reg(this, JOIN_CODE, this, &TrackeditActionsController::doGlobalJoin);
    dispatcher()->reg(this, DISJOIN_CODE, this, &TrackeditActionsController::doGlobalDisjoin);
    dispatcher()->reg(this, DUPLICATE_CODE, this, &TrackeditActionsController::doGlobalDuplicate);

    dispatcher()->reg(this, CUT_PER_CLIP_RIPPLE_CODE, this, &TrackeditActionsController::doGlobalCutPerClipRipple);
    dispatcher()->reg(this, CUT_PER_TRACK_RIPPLE_CODE, this, &TrackeditActionsController::doGlobalCutPerTrackRipple);
    dispatcher()->reg(this, CUT_ALL_TRACKS_RIPPLE_CODE, this, &TrackeditActionsController::doGlobalCutAllTracksRipple);

    dispatcher()->reg(this, PASTE_CODE, this, &TrackeditActionsController::paste);
    dispatcher()->reg(this, PASTE_INSERT_CODE, this, &TrackeditActionsController::pasteInsert);
    dispatcher()->reg(this, PASTE_INSERT_ALL_TRACKS_RIPPLE_CODE, this, &TrackeditActionsController::pasteInsertRipple);

    dispatcher()->reg(this, DELETE_PER_CLIP_RIPPLE_CODE, this, &TrackeditActionsController::doGlobalDeletePerClipRipple);
    dispatcher()->reg(this, DELETE_PER_TRACK_RIPPLE_CODE, this, &TrackeditActionsController::doGlobalDeletePerTrackRipple);
    dispatcher()->reg(this, DELETE_ALL_TRACKS_RIPPLE_CODE, this, &TrackeditActionsController::doGlobalDeleteAllTracksRipple);

    dispatcher()->reg(this, CLIP_CUT_CODE, this, &TrackeditActionsController::clipCut);
    dispatcher()->reg(this, MULTI_CLIP_CUT_CODE, this, &TrackeditActionsController::multiClipCut);
    dispatcher()->reg(this, RANGE_SELECTION_CUT_CODE, this, &TrackeditActionsController::rangeSelectionCut);

    dispatcher()->reg(this, CLIP_COPY_CODE, this, &TrackeditActionsController::clipCopy);
    dispatcher()->reg(this, MULTI_CLIP_COPY_CODE, this, &TrackeditActionsController::multiClipCopy);
    dispatcher()->reg(this, RANGE_SELECTION_COPY_CODE, this, &TrackeditActionsController::rangeSelectionCopy);

    dispatcher()->reg(this, CLIP_DELETE_CODE, this, &TrackeditActionsController::clipDelete);
    dispatcher()->reg(this, MULTI_CLIP_DELETE_CODE, this, &TrackeditActionsController::multiClipDelete);
    dispatcher()->reg(this, RANGE_SELECTION_DELETE_CODE, this, &TrackeditActionsController::rangeSelectionDelete);

    dispatcher()->reg(this, OPEN_CLIP_AND_SPEED_CODE, this, &TrackeditActionsController::openClipPitchAndSpeed);
    dispatcher()->reg(this, CLIP_RENDER_PITCH_AND_SPEED_CODE, this, &TrackeditActionsController::renderClipPitchAndSpeed);
    dispatcher()->reg(this, TRACK_SPLIT, this, &TrackeditActionsController::trackSplit);
    dispatcher()->reg(this, TRACK_SPLIT_AT, this, &TrackeditActionsController::tracksSplitAt);
    dispatcher()->reg(this, SPLIT_RANGE_SELECTION_AT_SILENCES, this, &TrackeditActionsController::splitRangeSelectionAtSilences);
    dispatcher()->reg(this, SPLIT_CLIPS_AT_SILENCES, this, &TrackeditActionsController::splitClipsAtSilences);
    dispatcher()->reg(this, SPLIT_RANGE_SELECTION_INTO_NEW_TRACKS, this, &TrackeditActionsController::splitRangeSelectionIntoNewTracks);
    dispatcher()->reg(this, SPLIT_CLIPS_INTO_NEW_TRACKS, this, &TrackeditActionsController::splitClipsIntoNewTracks);
    dispatcher()->reg(this, MERGE_SELECTED_ON_TRACK, this, &TrackeditActionsController::mergeSelectedOnTrack);
    dispatcher()->reg(this, UNDO, this, &TrackeditActionsController::undo);
    dispatcher()->reg(this, REDO, this, &TrackeditActionsController::redo);
    dispatcher()->reg(this, DUPLICATE_RANGE_SELECTION_CODE, this, &TrackeditActionsController::duplicateSelected);
    dispatcher()->reg(this, DUPLICATE_CLIPS_CODE, this, &TrackeditActionsController::duplicateClips);
    dispatcher()->reg(this, CLIP_SPLIT_CUT, this, &TrackeditActionsController::clipSplitCut);
    dispatcher()->reg(this, CLIP_SPLIT_DELETE, this, &TrackeditActionsController::clipSplitDelete);
    dispatcher()->reg(this, RANGE_SELECTION_SPLIT_CUT, this, &TrackeditActionsController::splitCutSelected);
    dispatcher()->reg(this, RANGE_SELECTION_SPLIT_DELETE, this, &TrackeditActionsController::splitDeleteSelected);
    dispatcher()->reg(this, "toggle-loop-region", this, &TrackeditActionsController::toggleLoopRegion);
    dispatcher()->reg(this, "clear-loop-region", this, &TrackeditActionsController::clearLoopRegion);
    dispatcher()->reg(this, "set-loop-region-to-selection", this, &TrackeditActionsController::setLoopRegionToSelection);
    dispatcher()->reg(this, "set-selection-to-loop", this, &TrackeditActionsController::setSelectionToLoop);
    dispatcher()->reg(this, "set-loop-region-in", this, &TrackeditActionsController::setLoopRegionIn);
    dispatcher()->reg(this, "set-loop-region-out", this, &TrackeditActionsController::setLoopRegionOut);
    dispatcher()->reg(this, NEW_MONO_TRACK, this, &TrackeditActionsController::newMonoTrack);
    dispatcher()->reg(this, NEW_STEREO_TRACK, this, &TrackeditActionsController::newStereoTrack);
    dispatcher()->reg(this, NEW_LABEL_TRACK, this, &TrackeditActionsController::newLabelTrack);
    dispatcher()->reg(this, TRACK_DELETE, this, &TrackeditActionsController::deleteTracks);
    dispatcher()->reg(this, TRACK_DUPLICATE_CODE, this, &TrackeditActionsController::duplicateTracks);
    dispatcher()->reg(this, TRACK_MOVE_UP, this, &TrackeditActionsController::moveTracksUp);
    dispatcher()->reg(this, TRACK_MOVE_DOWN, this, &TrackeditActionsController::moveTracksDown);
    dispatcher()->reg(this, TRACK_MOVE_TOP, this, &TrackeditActionsController::moveTracksToTop);
    dispatcher()->reg(this, TRACK_MOVE_BOTTOM, this, &TrackeditActionsController::moveTracksToBottom);
    dispatcher()->reg(this, TRACK_SWAP_CHANNELS, this, &TrackeditActionsController::swapStereoChannels);
    dispatcher()->reg(this, TRACK_SPLIT_STEREO_TO_LR, this, &TrackeditActionsController::splitStereoToLR);
    dispatcher()->reg(this, TRACK_SPLIT_STEREO_TO_CENTER, this, &TrackeditActionsController::splitStereoToCenter);
    dispatcher()->reg(this, TRACK_CHANGE_RATE_CUSTOM, this, &TrackeditActionsController::setCustomTrackRate);
    dispatcher()->reg(this, TRACK_MAKE_STEREO, this, &TrackeditActionsController::makeStereoTrack);
    dispatcher()->reg(this, TRACK_RESAMPLE, this, &TrackeditActionsController::resampleTracks);

    dispatcher()->reg(this, TRIM_AUDIO_OUTSIDE_SELECTION, this, &TrackeditActionsController::trimAudioOutsideSelection);
    dispatcher()->reg(this, SILENCE_AUDIO_SELECTION, this, &TrackeditActionsController::silenceAudioSelection);

    dispatcher()->reg(this, STRETCH_ENABLED_CODE, this, &TrackeditActionsController::toggleStretchClipToMatchTempo);

    dispatcher()->reg(this, GROUP_CLIPS_CODE, this, &TrackeditActionsController::groupClips);
    dispatcher()->reg(this, UNGROUP_CLIPS_CODE, this, &TrackeditActionsController::ungroupClips);

    dispatcher()->reg(this, AUTO_COLOR_QUERY, this, &TrackeditActionsController::setClipColor);
    dispatcher()->reg(this, CHANGE_COLOR_QUERY, this, &TrackeditActionsController::setClipColor);

    dispatcher()->reg(this, TRACK_CHANGE_COLOR_QUERY, this, &TrackeditActionsController::setTrackColor);
    dispatcher()->reg(this, TRACK_CHANGE_FORMAT_QUERY, this, &TrackeditActionsController::setTrackFormat);
    dispatcher()->reg(this, TRACK_CHANGE_RATE_QUERY, this, &TrackeditActionsController::setTrackRate);

    projectHistory()->historyChanged().onNotify(this, [this]() {
        notifyActionEnabledChanged(UNDO);
        notifyActionEnabledChanged(REDO);
    });

    globalContext()->isRecordingChanged().onNotify(this, [this]() {
        for (const auto& actionCode : actionsDisabledDuringRecording) {
            notifyActionEnabledChanged(actionCode);
        }
    });

    selectionController()->clipsSelected().onReceive(this, [this](const trackedit::ClipKeyList&) {
        notifyActionEnabledChanged(GROUP_CLIPS_CODE);
        notifyActionEnabledChanged(UNGROUP_CLIPS_CODE);
    });
}

bool TrackeditActionsController::actionEnabled(const muse::actions::ActionCode& actionCode) const
{
    if (!canReceiveAction(actionCode)) {
        return false;
    }

    return true;
}

muse::async::Channel<muse::actions::ActionCode> TrackeditActionsController::actionEnabledChanged() const
{
    return m_actionEnabledChanged;
}

void TrackeditActionsController::notifyActionEnabledChanged(const ActionCode& actionCode)
{
    m_actionEnabledChanged.send(actionCode);
}

void TrackeditActionsController::notifyActionCheckedChanged(const ActionCode& actionCode)
{
    m_actionCheckedChanged.send(actionCode);
}

void TrackeditActionsController::doGlobalCopy()
{
    if (selectionController()->timeSelectionIsNotEmpty()) {
        dispatcher()->dispatch(RANGE_SELECTION_COPY_CODE);
        return;
    }

    if (selectionController()->selectedClips().empty()) {
        return;
    }

    dispatcher()->dispatch(MULTI_CLIP_COPY_CODE);
}

void TrackeditActionsController::doGlobalCut()
{
    if (selectionController()->timeSelectionIsNotEmpty()) {
        auto selectedTracks = selectionController()->selectedTracks();
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        dispatcher()->dispatch(RANGE_SELECTION_SPLIT_CUT,
                               ActionData::make_arg3<TrackIdList, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));
        return;
    }

    if (selectionController()->selectedClips().size() > 1) {
        dispatcher()->dispatch(MULTI_CLIP_CUT_CODE, ActionData::make_arg1(false));
    } else if (!selectionController()->selectedClips().empty()) {
        ClipKey selectedClipKey = selectionController()->selectedClips().at(0);
        if (selectedClipKey.isValid()) {
            dispatcher()->dispatch(CLIP_SPLIT_CUT, ActionData::make_arg1<trackedit::ClipKey>(selectedClipKey));
            return;
        }
    }
}

void TrackeditActionsController::doGlobalCutPerClipRipple()
{
    auto moveClips = ActionData::make_arg1(false);
    if (selectionController()->timeSelectionIsNotEmpty()) {
        dispatcher()->dispatch(RANGE_SELECTION_CUT_CODE, moveClips);
        return;
    }

    if (selectionController()->selectedClips().empty()) {
        return;
    }

    dispatcher()->dispatch(MULTI_CLIP_CUT_CODE, moveClips);
}

void TrackeditActionsController::doGlobalCutPerTrackRipple()
{
    auto moveClips = ActionData::make_arg1(true);
    if (selectionController()->timeSelectionIsNotEmpty()) {
        dispatcher()->dispatch(RANGE_SELECTION_CUT_CODE, moveClips);
        return;
    }

    if (selectionController()->selectedClips().empty()) {
        return;
    }

    dispatcher()->dispatch(MULTI_CLIP_CUT_CODE, moveClips);
}

void TrackeditActionsController::doGlobalCutAllTracksRipple()
{
    if (selectionController()->timeSelectionIsNotEmpty()) {
        project::IAudacityProjectPtr project = globalContext()->currentProject();
        auto tracks = project->trackeditProject()->trackIdList();
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        trackeditInteraction()->clearClipboard();
        trackeditInteraction()->cutClipDataIntoClipboard(tracks, selectedStartTime, selectedEndTime, true);

        selectionController()->resetDataSelection();
        return;
    }

    if (selectionController()->selectedClips().empty()) {
        return;
    }

    dispatcher()->dispatch(MULTI_CLIP_CUT_CODE, ActionData::make_arg1(true));
}

void TrackeditActionsController::doGlobalDelete()
{
    if (selectionController()->timeSelectionIsNotEmpty()) {
        auto selectedTracks = selectionController()->selectedTracks();
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        dispatcher()->dispatch(RANGE_SELECTION_SPLIT_DELETE,
                               ActionData::make_arg3<TrackIdList, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));
        return;
    }

    if (!selectionController()->selectedClips().empty()) {
        dispatcher()->dispatch(MULTI_CLIP_DELETE_CODE, ActionData::make_arg1(false));

        return;
    }

    if (!selectionController()->selectedTracks().empty()) {
        dispatcher()->dispatch(TRACK_DELETE);
        return;
    }

    interactive()->error(muse::trc("trackedit", "No audio selected"),
                         muse::trc("trackedit", "Select the audio for Delete then try again."));
}

void TrackeditActionsController::doGlobalDeletePerClipRipple()
{
    auto moveClips = ActionData::make_arg1(false);

    if (selectionController()->timeSelectionIsNotEmpty()) {
        dispatcher()->dispatch(RANGE_SELECTION_DELETE_CODE, moveClips);
        return;
    }

    if (!selectionController()->selectedClips().empty()) {
        dispatcher()->dispatch(MULTI_CLIP_DELETE_CODE, moveClips);
        return;
    }

    interactive()->error(muse::trc("trackedit", "No audio selected"),
                         muse::trc("trackedit", "Select the audio for Delete then try again."));
}

void TrackeditActionsController::doGlobalDeletePerTrackRipple()
{
    auto moveClips = ActionData::make_arg1(true);

    if (selectionController()->timeSelectionIsNotEmpty()) {
        dispatcher()->dispatch(RANGE_SELECTION_DELETE_CODE, moveClips);
        return;
    }

    if (!selectionController()->selectedClips().empty()) {
        dispatcher()->dispatch(MULTI_CLIP_DELETE_CODE, moveClips);
        return;
    }
    interactive()->error(muse::trc("trackedit", "No audio selected"),
                         muse::trc("trackedit", "Select the audio for Delete then try again."));
}

void TrackeditActionsController::doGlobalDeleteAllTracksRipple()
{
    auto moveClips = ActionData::make_arg1(true);

    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto tracks = project->trackeditProject()->trackIdList();

    if (selectionController()->timeSelectionIsNotEmpty()) {
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        trackeditInteraction()->removeTracksData(tracks, selectedStartTime, selectedEndTime, true);

        selectionController()->resetDataSelection();
        return;
    }

    if (!selectionController()->selectedClips().empty()) {
        secs_t selectedStartTime = selectionController()->leftMostSelectedClipStartTime();
        secs_t selectedEndTime = selectionController()->rightMostSelectedClipEndTime();

        trackeditInteraction()->removeTracksData(tracks, selectedStartTime, selectedEndTime, true);

        selectionController()->resetDataSelection();
        return;
    }

    interactive()->error(muse::trc("trackedit", "No audio selected"),
                         muse::trc("trackedit", "Select the audio for Delete then try again."));
}

void TrackeditActionsController::doGlobalSplit()
{
    TrackIdList tracksIdsToSplit = selectionController()->selectedTracks();

    if (tracksIdsToSplit.empty()) {
        return;
    }

    std::vector<secs_t> pivots;
    if (selectionController()->timeSelectionIsNotEmpty()) {
        pivots.push_back(selectionController()->dataSelectedStartTime());
        pivots.push_back(selectionController()->dataSelectedEndTime());
    } else {
        pivots.push_back(playbackState()->playbackPosition());
    }

    dispatcher()->dispatch(TRACK_SPLIT_AT, ActionData::make_arg2<TrackIdList, std::vector<secs_t> >(tracksIdsToSplit, pivots));
}

void TrackeditActionsController::doGlobalSplitIntoNewTrack()
{
    if (selectionController()->timeSelectionIsNotEmpty()) {
        TrackIdList selectedTracks = selectionController()->selectedTracks();
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();
        dispatcher()->dispatch(SPLIT_RANGE_SELECTION_INTO_NEW_TRACKS,
                               ActionData::make_arg3<TrackIdList, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));
        return;
    }

    ClipKeyList selectedClips = selectionController()->selectedClips();
    if (selectedClips.empty()) {
        return;
    }

    dispatcher()->dispatch(SPLIT_CLIPS_INTO_NEW_TRACKS, ActionData::make_arg1<ClipKeyList>(selectedClips));
}

void TrackeditActionsController::doGlobalJoin()
{
    auto selectedTracks = selectionController()->selectedTracks();
    secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
    secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

    dispatcher()->dispatch(MERGE_SELECTED_ON_TRACK,
                           ActionData::make_arg3<TrackIdList, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));
}

void TrackeditActionsController::doGlobalDisjoin()
{
    if (selectionController()->timeSelectionIsNotEmpty()) {
        TrackIdList selectedTracks = selectionController()->selectedTracks();
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        dispatcher()->dispatch(SPLIT_RANGE_SELECTION_AT_SILENCES,
                               ActionData::make_arg3<TrackIdList, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));
        return;
    }

    ClipKeyList selectedClips = selectionController()->selectedClips();
    if (selectedClips.empty()) {
        return;
    }
    dispatcher()->dispatch(SPLIT_CLIPS_AT_SILENCES, ActionData::make_arg1<ClipKeyList>(selectedClips));
}

void TrackeditActionsController::undo()
{
    trackeditInteraction()->undo();
}

void TrackeditActionsController::redo()
{
    trackeditInteraction()->redo();
}

void TrackeditActionsController::doGlobalDuplicate()
{
    const auto selectedTracks = selectionController()->selectedTracks();

    if (!selectedTracks.empty()) {
        const auto selectedClips = selectionController()->selectedClips();
        if (selectedClips.empty()) {
            secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
            secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

            //If no range is selected, duplicate all content of the selected tracks
            //Otherwise, duplicate only the selected range
            (selectedStartTime == selectedEndTime)
            ? dispatcher()->dispatch(TRACK_DUPLICATE_CODE, ActionData::make_arg1<TrackIdList>(selectedTracks))
            : dispatcher()->dispatch(DUPLICATE_RANGE_SELECTION_CODE,
                                     ActionData::make_arg3<TrackIdList, secs_t, secs_t>(selectedTracks, selectedStartTime,
                                                                                        selectedEndTime));
        } else {
            dispatcher()->dispatch(DUPLICATE_CLIPS_CODE, ActionData::make_arg1<ClipKeyList>(selectedClips));
        }
    }
}

void TrackeditActionsController::clipCut(const ActionData& args)
{
    ClipKey clipKey = args.arg<ClipKey>(0);
    if (!clipKey.isValid()) {
        return;
    }

    trackeditInteraction()->clearClipboard();
    trackeditInteraction()->cutClipIntoClipboard(clipKey);
}

void TrackeditActionsController::clipCopy(const ActionData& args)
{
    ClipKey clipKey = args.arg<ClipKey>(0);
    if (!clipKey.isValid()) {
        return;
    }

    trackeditInteraction()->clearClipboard();
    trackeditInteraction()->copyClipIntoClipboard(clipKey);
}

void TrackeditActionsController::clipDelete(const ActionData& args)
{
    ClipKey clipKey = args.arg<ClipKey>(0);
    if (!clipKey.isValid()) {
        return;
    }

    selectionController()->resetSelectedClips();

    trackeditInteraction()->removeClip(clipKey);
}

void TrackeditActionsController::multiClipDelete(const ActionData& args)
{
    bool moveClips = false;

    if (args.count() >= 1) {
        moveClips = args.arg<bool>(0);
    }

    ClipKeyList selectedClipKeys = selectionController()->selectedClips();
    if (selectedClipKeys.empty()) {
        return;
    }

    selectionController()->resetSelectedClips();

    trackeditInteraction()->removeClips(selectedClipKeys, moveClips);
}

void TrackeditActionsController::multiClipCut(const ActionData& args)
{
    bool moveClips = false;

    if (args.count() >= 1) {
        moveClips = args.arg<bool>(0);
    }

    auto selectedClips = selectionController()->selectedClips();
    if (selectedClips.empty()) {
        return;
    }

    trackeditInteraction()->clearClipboard();
    multiClipCopy();
    selectionController()->resetSelectedClips();

    trackeditInteraction()->removeClips(selectedClips, moveClips);
}

void TrackeditActionsController::rangeSelectionCut(const ActionData& args)
{
    bool moveClips = false;

    if (args.count() >= 1) {
        moveClips = args.arg<bool>(0);
    }

    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->selectedTracks();
    secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
    secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

    trackeditInteraction()->clearClipboard();
    trackeditInteraction()->cutClipDataIntoClipboard(selectedTracks, selectedStartTime, selectedEndTime, moveClips);

    selectionController()->resetDataSelection();
}

void TrackeditActionsController::multiClipCopy()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->selectedTracks();
    auto selectedClips = selectionController()->selectedClips();
    auto tracks = project->trackeditProject()->trackList();

    trackeditInteraction()->clearClipboard();

    secs_t offset = 0.0;
    std::optional<secs_t> leftmostClipStartTime = trackeditInteraction()->getLeftmostClipStartTime(selectionController()->selectedClips());
    if (leftmostClipStartTime.has_value()) {
        offset = -leftmostClipStartTime.value();
    }

    for (const auto& track : tracks) {
        if (std::find(selectedTracks.begin(), selectedTracks.end(), track.id) == selectedTracks.end()) {
            continue;
        }

        ClipKeyList selectedTrackClips;
        for (const auto& clip : selectedClips) {
            if (clip.trackId == track.id) {
                selectedTrackClips.push_back(clip);
            }
        }

        trackeditInteraction()->copyNonContinuousTrackDataIntoClipboard(track.id, selectedTrackClips, offset);
    }
}

void TrackeditActionsController::rangeSelectionCopy()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->selectedTracks();
    secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
    secs_t selectedEndTime = selectionController()->dataSelectedEndTime();
    auto tracks = project->trackeditProject()->trackList();

    trackeditInteraction()->clearClipboard();

    for (const auto& track : tracks) {
        if (std::find(selectedTracks.begin(), selectedTracks.end(), track.id) == selectedTracks.end()) {
            continue;
        }

        trackeditInteraction()->copyContinuousTrackDataIntoClipboard(track.id, selectedStartTime, selectedEndTime);
    }
}

void TrackeditActionsController::rangeSelectionDelete(const ActionData& args)
{
    bool moveClips = false;

    if (args.count() >= 1) {
        moveClips = args.arg<bool>(0);
    }

    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->selectedTracks();
    auto selectedStartTime = selectionController()->dataSelectedStartTime();
    auto selectedEndTime = selectionController()->dataSelectedEndTime();

    if (selectedTracks.empty()) {
        return;
    }

    trackeditInteraction()->removeTracksData(selectedTracks, selectedStartTime, selectedEndTime, moveClips);

    selectionController()->resetDataSelection();
}

void TrackeditActionsController::paste()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto tracks = project->trackeditProject()->trackList();
    const double selectedStartTime = playbackState()->playbackPosition();

    if (!tracks.empty() && selectedStartTime >= 0) {
        auto ret = trackeditInteraction()->pasteFromClipboard(selectedStartTime, false);
        if (!ret && !ret.text().empty()) {
            interactive()->error(muse::trc("trackedit", "Paste error"), ret.text());
        }
    }
}

void TrackeditActionsController::pasteInsert()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto tracks = project->trackeditProject()->trackList();
    const double selectedStartTime = playbackState()->playbackPosition();

    if (!tracks.empty() && selectedStartTime >= 0) {
        auto ret = trackeditInteraction()->pasteFromClipboard(selectedStartTime, true);
        if (!ret && !ret.text().empty()) {
            interactive()->error(muse::trc("trackedit", "Paste error"), ret.text());
        }
    }
}

void TrackeditActionsController::pasteInsertRipple()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto tracks = project->trackeditProject()->trackList();
    const double selectedStartTime = playbackState()->playbackPosition();

    if (!tracks.empty() && selectedStartTime >= 0) {
        auto ret = trackeditInteraction()->pasteFromClipboard(selectedStartTime, false, true);
        if (!ret && !ret.text().empty()) {
            interactive()->error(muse::trc("trackedit", "Paste error"), ret.text());
        }
    }
}

void TrackeditActionsController::trackSplit(const ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    TrackId trackIdToSplit = args.arg<TrackId>(0);
    if (trackIdToSplit == -1) {
        return;
    }

    const secs_t playbackPosition = playbackState()->playbackPosition();

    dispatcher()->dispatch(TRACK_SPLIT_AT, ActionData::make_arg2<TrackIdList, secs_t>({ trackIdToSplit }, playbackPosition));
}

void TrackeditActionsController::tracksSplitAt(const ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 2) {
        return;
    }

    TrackIdList tracksIds = args.arg<TrackIdList>(0);

    if (tracksIds.empty()) {
        return;
    }

    auto pivot = args.arg<std::vector<secs_t> >(1);

    trackeditInteraction()->splitTracksAt(tracksIds, pivot);
}

void TrackeditActionsController::splitClipsAtSilences(const ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    ClipKeyList clipKeyList = args.arg<ClipKeyList>(0);
    if (clipKeyList.empty()) {
        return;
    }

    trackeditInteraction()->splitClipsAtSilences(clipKeyList);
}

void TrackeditActionsController::splitRangeSelectionAtSilences(const ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 3) {
        return;
    }

    TrackIdList tracksIds = args.arg<TrackIdList>(0);
    if (tracksIds.empty()) {
        return;
    }

    secs_t begin = args.arg<secs_t>(1);
    secs_t end = args.arg<secs_t>(2);

    trackeditInteraction()->splitRangeSelectionAtSilences(tracksIds, begin, end);
}

void TrackeditActionsController::splitRangeSelectionIntoNewTracks(const ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 3) {
        return;
    }

    TrackIdList tracksIds = args.arg<TrackIdList>(0);
    if (tracksIds.empty()) {
        return;
    }

    secs_t begin = args.arg<secs_t>(1);
    secs_t end = args.arg<secs_t>(2);

    trackeditInteraction()->splitRangeSelectionIntoNewTracks(tracksIds, begin, end);
}

void TrackeditActionsController::splitClipsIntoNewTracks(const ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    ClipKeyList clipKeyList = args.arg<ClipKeyList>(0);
    if (clipKeyList.empty()) {
        return;
    }

    trackeditInteraction()->splitClipsIntoNewTracks(clipKeyList);
}

void TrackeditActionsController::mergeSelectedOnTrack(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 3) {
        return;
    }

    TrackIdList tracksIds = args.arg<TrackIdList>(0);
    if (tracksIds.empty()) {
        return;
    }

    secs_t begin = args.arg<secs_t>(1);
    secs_t end = args.arg<secs_t>(2);

    trackeditInteraction()->mergeSelectedOnTracks(tracksIds, begin, end);
}

void TrackeditActionsController::duplicateSelected(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 3) {
        return;
    }

    TrackIdList tracksIds = args.arg<TrackIdList>(0);
    if (tracksIds.empty()) {
        return;
    }

    secs_t begin = args.arg<secs_t>(1);
    secs_t end = args.arg<secs_t>(2);

    trackeditInteraction()->duplicateSelectedOnTracks(tracksIds, begin, end);
}

void TrackeditActionsController::duplicateClips(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    ClipKeyList clipKeyList = args.arg<ClipKeyList>(0);
    trackeditInteraction()->duplicateClips(clipKeyList);
}

void TrackeditActionsController::clipSplitCut(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    ClipKey clipKey = args.arg<ClipKey>(0);
    if (!clipKey.isValid()) {
        return;
    }

    trackeditInteraction()->clearClipboard();
    trackeditInteraction()->clipSplitCut(clipKey);
}

void TrackeditActionsController::clipSplitDelete(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    ClipKey clipKey = args.arg<ClipKey>(0);
    if (!clipKey.isValid()) {
        return;
    }

    trackeditInteraction()->clipSplitDelete(clipKey);
}

void TrackeditActionsController::splitCutSelected(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 3) {
        return;
    }

    TrackIdList tracksIds = args.arg<TrackIdList>(0);
    if (tracksIds.empty()) {
        return;
    }

    secs_t begin = args.arg<secs_t>(1);
    secs_t end = args.arg<secs_t>(2);

    trackeditInteraction()->clearClipboard();
    trackeditInteraction()->splitCutSelectedOnTracks(tracksIds, begin, end);

    selectionController()->resetDataSelection();
}

void TrackeditActionsController::splitDeleteSelected(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 3) {
        return;
    }

    TrackIdList tracksIds = args.arg<TrackIdList>(0);
    if (tracksIds.empty()) {
        return;
    }

    secs_t begin = args.arg<secs_t>(1);
    secs_t end = args.arg<secs_t>(2);

    trackeditInteraction()->splitDeleteSelectedOnTracks(tracksIds, begin, end);

    selectionController()->resetDataSelection();
}

void TrackeditActionsController::toggleLoopRegion()
{
    NOT_IMPLEMENTED;
}

void TrackeditActionsController::clearLoopRegion()
{
    NOT_IMPLEMENTED;
}

void TrackeditActionsController::setLoopRegionToSelection()
{
    NOT_IMPLEMENTED;
}

void TrackeditActionsController::setSelectionToLoop()
{
    NOT_IMPLEMENTED;
}

void TrackeditActionsController::setLoopRegionIn()
{
    NOT_IMPLEMENTED;
}

void TrackeditActionsController::setLoopRegionOut()
{
    NOT_IMPLEMENTED;
}

void TrackeditActionsController::newMonoTrack()
{
    trackeditInteraction()->newMonoTrack();
}

void TrackeditActionsController::newStereoTrack()
{
    trackeditInteraction()->newStereoTrack();
}

void TrackeditActionsController::newLabelTrack()
{
    trackeditInteraction()->newLabelTrack();
}

void TrackeditActionsController::deleteTracks(const muse::actions::ActionData&)
{
    TrackIdList trackIds = selectionController()->selectedTracks();

    if (trackIds.empty()) {
        return;
    }

    trackeditInteraction()->deleteTracks(trackIds);
}

void TrackeditActionsController::duplicateTracks(const muse::actions::ActionData&)
{
    TrackIdList trackIds = selectionController()->selectedTracks();

    if (trackIds.empty()) {
        return;
    }

    trackeditInteraction()->duplicateTracks(trackIds);
}

void TrackeditActionsController::moveTracksUp(const muse::actions::ActionData&)
{
    TrackIdList trackIds = selectionController()->selectedTracks();

    if (trackIds.empty()) {
        return;
    }

    trackeditInteraction()->moveTracks(trackIds, TrackMoveDirection::Up);
}

void TrackeditActionsController::moveTracksDown(const muse::actions::ActionData&)
{
    TrackIdList trackIds = selectionController()->selectedTracks();

    if (trackIds.empty()) {
        return;
    }

    trackeditInteraction()->moveTracks(trackIds, TrackMoveDirection::Down);
}

void TrackeditActionsController::moveTracksToTop(const muse::actions::ActionData&)
{
    TrackIdList trackIds = selectionController()->selectedTracks();

    if (trackIds.empty()) {
        return;
    }

    trackeditInteraction()->moveTracks(trackIds, TrackMoveDirection::Top);
}

void TrackeditActionsController::moveTracksToBottom(const muse::actions::ActionData&)
{
    TrackIdList trackIds = selectionController()->selectedTracks();

    if (trackIds.empty()) {
        return;
    }

    trackeditInteraction()->moveTracks(trackIds, TrackMoveDirection::Bottom);
}

void TrackeditActionsController::swapStereoChannels(const muse::actions::ActionData&)
{
    const TrackIdList trackIds = selectionController()->selectedTracks();
    if (trackIds.empty()) {
        return;
    }

    trackeditInteraction()->swapStereoChannels(trackIds);
}

void TrackeditActionsController::splitStereoToLR(const muse::actions::ActionData&)
{
    const auto project = globalContext()->currentProject();
    if (!project) {
        return;
    }

    const TrackIdList selectedTracks = selectionController()->selectedTracks();
    if (selectedTracks.empty()) {
        return;
    }

    auto tracks = project->trackeditProject()->trackList();
    std::vector<TrackId> tracksIdsToSplit;
    for (const auto& track : tracks) {
        if (std::find(selectedTracks.begin(), selectedTracks.end(), track.id) == selectedTracks.end()) {
            continue;
        }

        tracksIdsToSplit.push_back(track.id);
    }

    trackeditInteraction()->splitStereoTracksToLRMono(tracksIdsToSplit);
}

void TrackeditActionsController::splitStereoToCenter(const muse::actions::ActionData&)
{
    const auto project = globalContext()->currentProject();
    if (!project) {
        return;
    }

    const TrackIdList selectedTracks = selectionController()->selectedTracks();
    if (selectedTracks.empty()) {
        return;
    }

    auto tracks = project->trackeditProject()->trackList();
    std::vector<TrackId> tracksIdsToSplit;
    for (const auto& track : tracks) {
        if (std::find(selectedTracks.begin(), selectedTracks.end(), track.id) == selectedTracks.end()) {
            continue;
        }

        tracksIdsToSplit.push_back(track.id);
    }

    trackeditInteraction()->splitStereoTracksToCenterMono(tracksIdsToSplit);
}

void TrackeditActionsController::trimAudioOutsideSelection()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->selectedTracks();
    auto selectedStartTime = selectionController()->dataSelectedStartTime();
    auto selectedEndTime = selectionController()->dataSelectedEndTime();
    auto tracks = project->trackeditProject()->trackList();

    std::vector<TrackId> tracksIdsToTrim;
    for (const auto& track : tracks) {
        if (std::find(selectedTracks.begin(), selectedTracks.end(), track.id) == selectedTracks.end()) {
            continue;
        }

        tracksIdsToTrim.push_back(track.id);
    }

    if (tracksIdsToTrim.empty()) {
        return;
    }

    trackeditInteraction()->trimTracksData(tracksIdsToTrim, selectedStartTime, selectedEndTime);
}

void TrackeditActionsController::silenceAudioSelection()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->selectedTracks();
    auto selectedStartTime = selectionController()->dataSelectedStartTime();
    auto selectedEndTime = selectionController()->dataSelectedEndTime();
    auto tracks = project->trackeditProject()->trackList();

    std::vector<TrackId> tracksIdsToSilence;
    for (const auto& track : tracks) {
        if (std::find(selectedTracks.begin(), selectedTracks.end(), track.id) == selectedTracks.end()) {
            continue;
        }

        tracksIdsToSilence.push_back(track.id);
    }

    if (tracksIdsToSilence.empty()) {
        return;
    }

    trackeditInteraction()->silenceTracksData(tracksIdsToSilence, selectedStartTime, selectedEndTime);
}

void TrackeditActionsController::toggleStretchClipToMatchTempo(const ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    trackedit::ClipKey clipKey = args.arg<trackedit::ClipKey>(0);
    if (!clipKey.isValid()) {
        return;
    }

    trackeditInteraction()->toggleStretchToMatchProjectTempo(clipKey);
    notifyActionCheckedChanged(STRETCH_ENABLED_CODE);
}

void TrackeditActionsController::openClipPitchAndSpeed()
{
    auto selectedClips = selectionController()->selectedClips();

    if (selectedClips.empty() || selectedClips.size() > 1) {
        return;
    }

    dispatcher()->dispatch("clip-pitch-speed", ActionData::make_arg1<trackedit::ClipKey>(selectedClips.front()));
}

void TrackeditActionsController::renderClipPitchAndSpeed(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    trackedit::ClipKey clipKey = args.arg<trackedit::ClipKey>(0);
    if (!clipKey.isValid()) {
        return;
    }

    interactive()->showProgress(muse::trc("trackedit", "Applying"), trackeditInteraction()->progress().get());

    trackeditInteraction()->renderClipPitchAndSpeed(clipKey);
}

void TrackeditActionsController::groupClips()
{
    const auto selectedClips = selectionController()->selectedClips();

    trackeditInteraction()->groupClips(selectedClips);

    notifyActionEnabledChanged(GROUP_CLIPS_CODE);
    notifyActionEnabledChanged(UNGROUP_CLIPS_CODE);
}

void TrackeditActionsController::ungroupClips()
{
    trackeditInteraction()->ungroupClips(selectionController()->selectedClips());

    notifyActionEnabledChanged(GROUP_CLIPS_CODE);
    notifyActionEnabledChanged(UNGROUP_CLIPS_CODE);
}

void TrackeditActionsController::setClipColor(const muse::actions::ActionQuery& q)
{
    if (!selectionController()->hasSelectedClips()) {
        return;
    }

    std::string newColor;
    if (q.contains("color")) {
        newColor = q.param("color").toString();
    } else {
        newColor = "";
    }

    auto clipKey = selectionController()->selectedClips().front();
    trackeditInteraction()->changeClipColor(clipKey, newColor);
    notifyActionCheckedChanged(q.toString());
}

void TrackeditActionsController::setTrackColor(const muse::actions::ActionQuery& q)
{
    const auto tracks = selectionController()->selectedTracks();
    if (tracks.empty()) {
        return;
    }

    std::string color;
    if (q.contains("color")) {
        color = q.param("color").toString();
    } else {
        color = "";
    }

    trackeditInteraction()->changeTracksColor(tracks, color);
    notifyActionCheckedChanged(q.toString());
}

void TrackeditActionsController::setTrackFormat(const muse::actions::ActionQuery& q)
{
    const auto tracks = selectionController()->selectedTracks();
    if (tracks.empty()) {
        return;
    }

    if (!q.contains("format")) {
        return;
    }

    const int format = q.param("format").toInt();
    if (trackeditInteraction()->changeTracksFormat(tracks, static_cast<TrackFormat>(format))) {
        notifyActionCheckedChanged(q.toString());
    }
}

void TrackeditActionsController::setCustomTrackRate(const muse::actions::ActionData&)
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return;
    }

    const TrackIdList tracks = selectionController()->selectedTracks();
    if (tracks.empty()) {
        return;
    }

    const TrackId focusedTrackId = selectionController()->focusedTrack();
    const std::optional<Track> focused = project->trackeditProject()->track(focusedTrackId);
    if (!focused) {
        return;
    }

    muse::UriQuery customRateUri("audacity://trackedit/custom_rate");
    customRateUri.addParam("title", muse::Val(muse::trc("trackedit", "Set rate")));
    customRateUri.addParam("rate", muse::Val(static_cast<int>(focused.value().rate)));

    RetVal<Val> rv = interactive()->openSync(customRateUri);
    if (rv.ret.code() != static_cast<int>(Ret::Code::Ok)) {
        return;
    }

    const auto customRate = rv.val.toInt();
    if (customRate <= 0) {
        return;
    }

    trackeditInteraction()->changeTracksRate(tracks, customRate);
}

void TrackeditActionsController::setTrackRate(const muse::actions::ActionQuery& q)
{
    const auto tracks = selectionController()->selectedTracks();
    if (tracks.empty()) {
        return;
    }

    if (!q.contains("rate")) {
        return;
    }

    const int rate = q.param("rate").toInt();
    if (trackeditInteraction()->changeTracksRate(tracks, rate)) {
        notifyActionCheckedChanged(q.toString());
    }
}

void TrackeditActionsController::makeStereoTrack(const muse::actions::ActionData&)
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return;
    }

    const auto selectedTracks = selectionController()->selectedTracks();
    if (selectedTracks.size() != 1) {
        return;
    }

    const std::optional<Track> selectedTrack = project->trackeditProject()->track(selectionController()->selectedTracks().front());
    if (!selectedTrack || selectedTrack->type != TrackType::Mono) {
        return;
    }

    const TrackList tracks = project->trackeditProject()->trackList();
    const auto it = std::find_if(tracks.begin(), tracks.end(),
                                 [&selectedTrack](const Track& track) { return track.id == selectedTrack->id; });

    if (it == tracks.end()) {
        return;
    }

    const auto nextTrack = std::next(it);
    if ((nextTrack == tracks.end()) || nextTrack->type != TrackType::Mono) {
        return;
    }

    trackeditInteraction()->makeStereoTrack(selectedTrack->id, nextTrack->id);
}

void TrackeditActionsController::resampleTracks(const muse::actions::ActionData&)
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return;
    }

    const TrackIdList selectedTracks = selectionController()->selectedTracks();
    if (selectedTracks.size() == 0) {
        return;
    }

    const TrackId focusedTrackId = selectionController()->focusedTrack();
    const std::optional<Track> focused = project->trackeditProject()->track(focusedTrackId);
    if (!focused) {
        return;
    }

    muse::UriQuery resampleUri("audacity://trackedit/custom_rate");

    muse::ValList availableSampleRates;
    for (const auto& rate : audioDevicesProvider()->availableSampleRateList()) {
        availableSampleRates.push_back(muse::Val(static_cast<int>(rate)));
    }
    resampleUri.addParam("availableRates", muse::Val(availableSampleRates));
    resampleUri.addParam("title", muse::Val(muse::trc("trackedit", "Resample")));
    resampleUri.addParam("rate", muse::Val(static_cast<int>(focused.value().rate)));

    const RetVal<Val> rv = interactive()->openSync(resampleUri);
    if (rv.ret.code() != static_cast<int>(Ret::Code::Ok)) {
        return;
    }

    const int customRate = rv.val.toInt();
    if (customRate <= 0) {
        return;
    }

    trackeditInteraction()->resampleTracks(selectedTracks, customRate);
}

bool TrackeditActionsController::actionChecked(const ActionCode&) const
{
    //! TODO AU4
    return false;
}

Channel<ActionCode> TrackeditActionsController::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}

bool TrackeditActionsController::canReceiveAction(const ActionCode& actionCode) const
{
    if (globalContext()->currentProject() == nullptr) {
        return false;
    } else if (globalContext()->isRecording() && muse::contains(actionsDisabledDuringRecording, actionCode)) {
        return false;
    } else if (actionCode == UNDO) {
        return trackeditInteraction()->canUndo();
    } else if (actionCode == REDO) {
        return trackeditInteraction()->canRedo();
    } else if (actionCode == GROUP_CLIPS_CODE) {
        return selectionController()->selectedClips().size() > 1 && !selectionController()->isSelectionGrouped();
    } else if (actionCode == UNGROUP_CLIPS_CODE) {
        return selectionController()->selectedClips().size() > 1 && selectionController()->selectionContainsGroup();
    }

    return true;
}

au::context::IPlaybackStatePtr TrackeditActionsController::playbackState() const
{
    return globalContext()->playbackState();
}
