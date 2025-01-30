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
static const ActionCode DELETE_CODE("delete");
static const ActionCode SPLIT_CUT_CODE("split-cut");
static const ActionCode SPLIT_DELETE_CODE("split-delete");
static const ActionCode SPLIT_CODE("split");
static const ActionCode JOIN_CODE("join");
static const ActionCode DUPLICATE_CODE("duplicate");

static const ActionCode CLIP_CUT_CODE("clip-cut");
static const ActionCode MULTI_CLIP_CUT_CODE("multi-clip-cut");
static const ActionCode RANGE_SELECTION_CUT_CODE("clip-cut-selected");

static const ActionCode CLIP_COPY_CODE("clip-copy");
static const ActionCode MULTI_CLIP_COPY_CODE("multi-clip-copy");
static const ActionCode RANGE_SELECTION_COPY_CODE("clip-copy-selected");

static const ActionCode CLIP_DELETE_CODE("clip-delete");
static const ActionCode MULTI_CLIP_DELETE_CODE("multi-clip-delete");
static const ActionCode RANGE_SELECTION_DELETE_CODE("clip-delete-selected");

static const ActionCode CLIP_RENDER_PITCH_AND_SPEED_CODE("clip-render-pitch-speed");
static const ActionCode PASTE("paste");
static const ActionCode TRACK_SPLIT("track-split");
static const ActionCode TRACK_SPLIT_AT("track-split-at");
static const ActionCode MERGE_SELECTED_ON_TRACK("merge-selected-on-tracks");
static const ActionCode DUPLICATE_RANGE_SELECTION_CODE("duplicate-selected");
static const ActionCode DUPLICATE_CLIPS_CODE("duplicate-clips");
static const ActionCode CLIP_SPLIT_CUT("clip-split-cut");
static const ActionCode CLIP_SPLIT_DELETE("clip-split-delete");
static const ActionCode SPLIT_CUT_SELECTED("split-cut-selected");
static const ActionCode SPLIT_DELETE_SELECTED("split-delete-selected");
static const ActionCode NEW_MONO_TRACK("new-mono-track");
static const ActionCode NEW_STEREO_TRACK("new-stereo-track");
static const ActionCode NEW_LABEL_TRACK("new-label-track");
static const ActionCode TRACK_DELETE("track-delete");
static const ActionCode TRACK_DUPLICATE_CODE("track-duplicate");
static const ActionCode TRACK_MOVE_UP("track-move-up");
static const ActionCode TRACK_MOVE_DOWN("track-move-down");
static const ActionCode TRACK_MOVE_TOP("track-move-top");
static const ActionCode TRACK_MOVE_BOTTOM("track-move-bottom");

static const ActionCode TRIM_AUDIO_OUTSIDE_SELECTION("trim-audio-outside-selection");
static const ActionCode SILENCE_AUDIO_SELECTION("silence-audio-selection");

static const ActionCode UNDO("undo");
static const ActionCode REDO("redo");

static const ActionCode STRETCH_ENABLED_CODE("stretch-clip-to-match-tempo");

static const ActionCode GROUP_CLIPS_CODE("group-clips");
static const ActionCode UNGROUP_CLIPS_CODE("ungroup-clips");

static const ActionQuery AUTO_COLOR_QUERY("action://trackedit/clip/change-color-auto");
static const ActionQuery CHANGE_COLOR_QUERY("action://trackedit/clip/change-color");

// In principle, disabled are actions that modify the data involved in playback.
static const std::vector<ActionCode> actionsDisabledDuringRecording {
    CUT_CODE,
    DELETE_CODE,
    SPLIT_CUT_CODE,
    SPLIT_DELETE_CODE,
    SPLIT_CODE,
    JOIN_CODE,
    DUPLICATE_CODE,
    CLIP_CUT_CODE,
    MULTI_CLIP_CUT_CODE,
    RANGE_SELECTION_CUT_CODE,
    CLIP_DELETE_CODE,
    MULTI_CLIP_DELETE_CODE,
    RANGE_SELECTION_DELETE_CODE,
    CLIP_RENDER_PITCH_AND_SPEED_CODE,
    PASTE,
    TRACK_SPLIT,
    TRACK_SPLIT_AT,
    MERGE_SELECTED_ON_TRACK,
    DUPLICATE_RANGE_SELECTION_CODE,
    DUPLICATE_CLIPS_CODE,
    CLIP_SPLIT_CUT,
    CLIP_SPLIT_DELETE,
    SPLIT_CUT_SELECTED,
    SPLIT_DELETE_SELECTED,
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
    GROUP_CLIPS_CODE,
    UNGROUP_CLIPS_CODE
};

void TrackeditActionsController::init()
{
    dispatcher()->reg(this, COPY_CODE, this, &TrackeditActionsController::doGlobalCopy);
    dispatcher()->reg(this, CUT_CODE, this, &TrackeditActionsController::doGlobalCut);
    dispatcher()->reg(this, DELETE_CODE, this, &TrackeditActionsController::doGlobalDelete);
    dispatcher()->reg(this, SPLIT_CUT_CODE, this, &TrackeditActionsController::doGlobalSplitCut);
    dispatcher()->reg(this, SPLIT_DELETE_CODE, this, &TrackeditActionsController::doGlobalSplitDelete);
    dispatcher()->reg(this, SPLIT_CODE, this, &TrackeditActionsController::doGlobalSplit);
    dispatcher()->reg(this, JOIN_CODE, this, &TrackeditActionsController::doGlobalJoin);
    dispatcher()->reg(this, DUPLICATE_CODE, this, &TrackeditActionsController::doGlobalDuplicate);

    dispatcher()->reg(this, CLIP_CUT_CODE, this, &TrackeditActionsController::clipCut);
    dispatcher()->reg(this, MULTI_CLIP_CUT_CODE, this, &TrackeditActionsController::multiClipCut);
    dispatcher()->reg(this, RANGE_SELECTION_CUT_CODE, this, &TrackeditActionsController::rangeSelectionCut);

    dispatcher()->reg(this, CLIP_COPY_CODE, this, &TrackeditActionsController::clipCopy);
    dispatcher()->reg(this, MULTI_CLIP_COPY_CODE, this, &TrackeditActionsController::multiClipCopy);
    dispatcher()->reg(this, RANGE_SELECTION_COPY_CODE, this, &TrackeditActionsController::rangeSelectionCopy);

    dispatcher()->reg(this, CLIP_DELETE_CODE, this, &TrackeditActionsController::clipDelete);
    dispatcher()->reg(this, MULTI_CLIP_DELETE_CODE, this, &TrackeditActionsController::multiClipDelete);
    dispatcher()->reg(this, RANGE_SELECTION_DELETE_CODE, this, &TrackeditActionsController::rangeSelectionDelete);

    dispatcher()->reg(this, CLIP_RENDER_PITCH_AND_SPEED_CODE, this, &TrackeditActionsController::renderClipPitchAndSpeed);
    dispatcher()->reg(this, PASTE, this, &TrackeditActionsController::paste);
    dispatcher()->reg(this, TRACK_SPLIT, this, &TrackeditActionsController::trackSplit);
    dispatcher()->reg(this, TRACK_SPLIT_AT, this, &TrackeditActionsController::tracksSplitAt);
    dispatcher()->reg(this, MERGE_SELECTED_ON_TRACK, this, &TrackeditActionsController::mergeSelectedOnTrack);
    dispatcher()->reg(this, UNDO, this, &TrackeditActionsController::undo);
    dispatcher()->reg(this, REDO, this, &TrackeditActionsController::redo);
    dispatcher()->reg(this, DUPLICATE_RANGE_SELECTION_CODE, this, &TrackeditActionsController::duplicateSelected);
    dispatcher()->reg(this, DUPLICATE_CLIPS_CODE, this, &TrackeditActionsController::duplicateClips);
    dispatcher()->reg(this, CLIP_SPLIT_CUT, this, &TrackeditActionsController::clipSplitCut);
    dispatcher()->reg(this, CLIP_SPLIT_DELETE, this, &TrackeditActionsController::clipSplitDelete);
    dispatcher()->reg(this, SPLIT_CUT_SELECTED, this, &TrackeditActionsController::splitCutSelected);
    dispatcher()->reg(this, SPLIT_DELETE_SELECTED, this, &TrackeditActionsController::splitDeleteSelected);
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

    dispatcher()->reg(this, TRIM_AUDIO_OUTSIDE_SELECTION, this, &TrackeditActionsController::trimAudioOutsideSelection);
    dispatcher()->reg(this, SILENCE_AUDIO_SELECTION, this, &TrackeditActionsController::silenceAudioSelection);

    dispatcher()->reg(this, STRETCH_ENABLED_CODE, this, &TrackeditActionsController::toggleStretchClipToMatchTempo);

    dispatcher()->reg(this, GROUP_CLIPS_CODE, this, &TrackeditActionsController::groupClips);
    dispatcher()->reg(this, UNGROUP_CLIPS_CODE, this, &TrackeditActionsController::ungroupClips);

    dispatcher()->reg(this, AUTO_COLOR_QUERY, this, &TrackeditActionsController::setClipColor);
    dispatcher()->reg(this, CHANGE_COLOR_QUERY, this, &TrackeditActionsController::setClipColor);

    projectHistory()->isUndoRedoAvailableChanged().onNotify(this, [this]() {
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
        dispatcher()->dispatch(RANGE_SELECTION_CUT_CODE);
        return;
    }

    if (selectionController()->selectedClips().empty()) {
        return;
    }

    dispatcher()->dispatch(MULTI_CLIP_CUT_CODE);
}

void TrackeditActionsController::doGlobalDelete()
{
    if (selectionController()->timeSelectionIsNotEmpty()) {
        dispatcher()->dispatch(RANGE_SELECTION_DELETE_CODE);
        return;
    }

    if (selectionController()->selectedClips().empty()) {
        return;
    }

    dispatcher()->dispatch(MULTI_CLIP_DELETE_CODE);
}

void TrackeditActionsController::doGlobalSplitCut()
{
    if (selectionController()->timeSelectionIsNotEmpty()) {
        auto selectedTracks = selectionController()->selectedTracks();
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        dispatcher()->dispatch(SPLIT_CUT_SELECTED,
                               ActionData::make_arg3<TrackIdList, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));
        return;
    }

    if (selectionController()->selectedClips().empty()) {
        interactive()->error(muse::trc("trackedit", "No audio selected"),
                             muse::trc("trackedit", "Select the audio for Split Cut to use then try again."));
        return;
    }

    if (selectionController()->selectedClips().size() == 1) {
        ClipKey selectedClipKey = selectionController()->selectedClips().at(0);
        if (selectedClipKey.isValid()) {
            dispatcher()->dispatch(CLIP_SPLIT_CUT, ActionData::make_arg1<trackedit::ClipKey>(selectedClipKey));
            return;
        }
    }
}

void TrackeditActionsController::doGlobalSplitDelete()
{
    if (selectionController()->timeSelectionIsNotEmpty()) {
        auto selectedTracks = selectionController()->selectedTracks();
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        dispatcher()->dispatch(SPLIT_DELETE_SELECTED,
                               ActionData::make_arg3<TrackIdList, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));
        return;
    }

    if (selectionController()->selectedClips().empty()) {
        interactive()->error(muse::trc("trackedit", "No audio selected"),
                             muse::trc("trackedit", "Select the audio for Split Cut to use then try again."));
        return;
    }

    if (selectionController()->selectedClips().size() == 1) {
        ClipKey selectedClipKey = selectionController()->selectedClips().at(0);
        if (selectedClipKey.isValid()) {
            dispatcher()->dispatch(CLIP_SPLIT_DELETE, ActionData::make_arg1<trackedit::ClipKey>(selectedClipKey));
            return;
        }
    }
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
        pivots.push_back(globalContext()->playbackState()->playbackPosition());
    }

    for (const auto& pivot : pivots) {
        dispatcher()->dispatch(TRACK_SPLIT_AT, ActionData::make_arg2<TrackIdList, secs_t>(tracksIdsToSplit, pivot));
    }
}

void TrackeditActionsController::doGlobalJoin()
{
    auto selectedTracks = selectionController()->selectedTracks();
    secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
    secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

    dispatcher()->dispatch(MERGE_SELECTED_ON_TRACK,
                           ActionData::make_arg3<TrackIdList, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));
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

void TrackeditActionsController::multiClipDelete()
{
    ClipKeyList selectedClipKeys = selectionController()->selectedClips();
    if (selectedClipKeys.empty()) {
        return;
    }

    selectionController()->resetSelectedClips();

    trackeditInteraction()->removeClips(selectedClipKeys);
}

void TrackeditActionsController::multiClipCut()
{
    auto selectedClips = selectionController()->selectedClips();
    if (selectedClips.empty()) {
        return;
    }

    trackeditInteraction()->clearClipboard();
    multiClipCopy();
    selectionController()->resetSelectedClips();
    trackeditInteraction()->removeClips(selectedClips);
}

void TrackeditActionsController::rangeSelectionCut()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->selectedTracks();
    secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
    secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

    trackeditInteraction()->clearClipboard();
    trackeditInteraction()->cutClipDataIntoClipboard(selectedTracks, selectedStartTime, selectedEndTime);

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

void TrackeditActionsController::rangeSelectionDelete()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->selectedTracks();
    auto selectedStartTime = selectionController()->dataSelectedStartTime();
    auto selectedEndTime = selectionController()->dataSelectedEndTime();

    if (selectedTracks.empty()) {
        return;
    }

    trackeditInteraction()->removeTracksData(selectedTracks, selectedStartTime, selectedEndTime);

    selectionController()->resetDataSelection();
}

void TrackeditActionsController::paste()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto tracks = project->trackeditProject()->trackList();
    double selectedStartTime = globalContext()->playbackState()->playbackPosition();

    if (!tracks.empty() && selectedStartTime >= 0) {
        auto ret = trackeditInteraction()->pasteFromClipboard(selectedStartTime);
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

    secs_t playbackPosition = globalContext()->playbackState()->playbackPosition();

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

    secs_t playbackPosition = args.arg<secs_t>(1);

    trackeditInteraction()->splitTracksAt(tracksIds, playbackPosition);
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
    secs_t duration = end - begin;

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

void TrackeditActionsController::moveTracksUp(const muse::actions::ActionData& args)
{
    TrackIdList trackIds = selectionController()->selectedTracks();

    if (trackIds.empty()) {
        return;
    }

    trackeditInteraction()->moveTracks(trackIds, TrackMoveDirection::Up);
}

void TrackeditActionsController::moveTracksDown(const muse::actions::ActionData& args)
{
    TrackIdList trackIds = selectionController()->selectedTracks();

    if (trackIds.empty()) {
        return;
    }

    trackeditInteraction()->moveTracks(trackIds, TrackMoveDirection::Down);
}

void TrackeditActionsController::moveTracksToTop(const muse::actions::ActionData& args)
{
    TrackIdList trackIds = selectionController()->selectedTracks();

    if (trackIds.empty()) {
        return;
    }

    trackeditInteraction()->moveTracks(trackIds, TrackMoveDirection::Top);
}

void TrackeditActionsController::moveTracksToBottom(const muse::actions::ActionData& args)
{
    TrackIdList trackIds = selectionController()->selectedTracks();

    if (trackIds.empty()) {
        return;
    }

    trackeditInteraction()->moveTracks(trackIds, TrackMoveDirection::Bottom);
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

bool TrackeditActionsController::actionChecked(const ActionCode& actionCode) const
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
