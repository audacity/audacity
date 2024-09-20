/*
* Audacity: A Digital Audio Editor
*/
#include "trackeditactionscontroller.h"
#include "project/internal/audacityproject.h"
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
static const ActionCode CLIP_COPY_CODE("clip-copy");
static const ActionCode CLIP_DELETE_CODE("clip-delete");
static const ActionCode CLIP_CUT_SELECTED_CODE("clip-cut-selected");
static const ActionCode CLIP_COPY_SELECTED_CODE("clip-copy-selected");
static const ActionCode CLIP_DELETE_SELECTED_CODE("clip-delete-selected");
static const ActionCode PASTE("paste");
static const ActionCode TRACK_SPLIT("track-split");
static const ActionCode TRACK_SPLIT_AT("track-split-at");
static const ActionCode MERGE_SELECTED_ON_TRACK("merge-selected-on-tracks");
static const ActionCode DUPLICATE_SELECTED("duplicate-selected");
static const ActionCode DUPLICATE_CLIP("duplicate-clip");
static const ActionCode CLIP_SPLIT_CUT("clip-split-cut");
static const ActionCode CLIP_SPLIT_DELETE("clip-split-delete");
static const ActionCode SPLIT_CUT_SELECTED("split-cut-selected");
static const ActionCode SPLIT_DELETE_SELECTED("split-delete-selected");
static const ActionCode NEW_MONO_TRACK("new-mono-track");
static const ActionCode NEW_STEREO_TRACK("new-stereo-track");
static const ActionCode NEW_LABEL_TRACK("new-label-track");

static const ActionCode TRIM_AUDIO_OUTSIDE_SELECTION("trim-audio-outside-selection");
static const ActionCode SILENCE_AUDIO_SELECTION("silence-audio-selection");

static const ActionCode UNDO("undo");
static const ActionCode REDO("redo");

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
    dispatcher()->reg(this, CLIP_COPY_CODE, this, &TrackeditActionsController::clipCopy);
    dispatcher()->reg(this, CLIP_DELETE_CODE, this, &TrackeditActionsController::clipDelete);
    dispatcher()->reg(this, CLIP_CUT_SELECTED_CODE, this, &TrackeditActionsController::clipCutSelected);
    dispatcher()->reg(this, CLIP_COPY_SELECTED_CODE, this, &TrackeditActionsController::clipCopySelected);
    dispatcher()->reg(this, CLIP_DELETE_SELECTED_CODE, this, &TrackeditActionsController::clipDeleteSelected);
    dispatcher()->reg(this, PASTE, this, &TrackeditActionsController::paste);
    dispatcher()->reg(this, TRACK_SPLIT, this, &TrackeditActionsController::trackSplit);
    dispatcher()->reg(this, TRACK_SPLIT_AT, this, &TrackeditActionsController::trackSplitAt);
    dispatcher()->reg(this, MERGE_SELECTED_ON_TRACK, this, &TrackeditActionsController::mergeSelectedOnTrack);
    dispatcher()->reg(this, UNDO, this, &TrackeditActionsController::undo);
    dispatcher()->reg(this, REDO, this, &TrackeditActionsController::redo);
    dispatcher()->reg(this, DUPLICATE_SELECTED, this, &TrackeditActionsController::duplicateSelected);
    dispatcher()->reg(this, DUPLICATE_CLIP, this, &TrackeditActionsController::duplicateClip);
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

    dispatcher()->reg(this, TRIM_AUDIO_OUTSIDE_SELECTION, this, &TrackeditActionsController::trimAudioOutsideSelection);
    dispatcher()->reg(this, SILENCE_AUDIO_SELECTION, this, &TrackeditActionsController::silenceAudioSelection);
}

void TrackeditActionsController::notifyActionCheckedChanged(const ActionCode& actionCode)
{
    m_actionCheckedChanged.send(actionCode);
}

void TrackeditActionsController::doGlobalCopy()
{
    if (selectionController()->isDataSelected()) {
        dispatcher()->dispatch(CLIP_COPY_SELECTED_CODE);
        return;
    }

    ClipKey selectedClipKey = selectionController()->selectedClip();
    if (selectedClipKey.isValid()) {
        dispatcher()->dispatch(CLIP_COPY_CODE, ActionData::make_arg1<trackedit::ClipKey>(selectedClipKey));
    }
}

void TrackeditActionsController::doGlobalCut()
{
    if (selectionController()->isDataSelected()) {
        dispatcher()->dispatch(CLIP_CUT_SELECTED_CODE);
        return;
    }

    ClipKey selectedClipKey = selectionController()->selectedClip();
    if (selectedClipKey.isValid()) {
        dispatcher()->dispatch(CLIP_CUT_CODE, ActionData::make_arg1<trackedit::ClipKey>(selectedClipKey));
    }
}

void TrackeditActionsController::doGlobalDelete()
{
    if (selectionController()->isDataSelected()) {
        dispatcher()->dispatch(CLIP_DELETE_SELECTED_CODE);
        return;
    }

    ClipKey selectedClipKey = selectionController()->selectedClip();
    if (selectedClipKey.isValid()) {
        dispatcher()->dispatch(CLIP_DELETE_CODE, ActionData::make_arg1<trackedit::ClipKey>(selectedClipKey));
    }
}

void TrackeditActionsController::doGlobalSplitCut()
{
    if (selectionController()->isDataSelected()) {
        auto selectedTracks = selectionController()->dataSelectedOnTracks();
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        dispatcher()->dispatch(SPLIT_CUT_SELECTED,
                           ActionData::make_arg3<std::vector<TrackId>, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));
        return;
    }

    ClipKey selectedClipKey = selectionController()->selectedClip();
    if (selectedClipKey.isValid()) {
        dispatcher()->dispatch(CLIP_SPLIT_CUT, ActionData::make_arg1<trackedit::ClipKey>(selectedClipKey));
        return;
    }

    interactive()->error(muse::trc("no_audio", "No audio selected"), std::string("Select the audio for Split Cut to use then try again."));
}

void TrackeditActionsController::doGlobalSplitDelete()
{
    if (selectionController()->isDataSelected()) {
        auto selectedTracks = selectionController()->dataSelectedOnTracks();
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        dispatcher()->dispatch(SPLIT_DELETE_SELECTED,
                           ActionData::make_arg3<std::vector<TrackId>, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));
        return;
    }

    ClipKey selectedClipKey = selectionController()->selectedClip();
    if (selectedClipKey.isValid()) {
        dispatcher()->dispatch(CLIP_SPLIT_DELETE, ActionData::make_arg1<trackedit::ClipKey>(selectedClipKey));
        return;
    }

    interactive()->error(muse::trc("no_audio", "No audio selected"), std::string("Select the audio for Split Cut to use then try again."));
}

void TrackeditActionsController::doGlobalSplit()
{
    TrackId trackIdToSplit = selectionController()->selectedTrack();
    if (trackIdToSplit == -1) {
        trackIdToSplit = selectionController()->selectedClip().trackId;
    }

    if (trackIdToSplit == -1) {
        return;
    }

    secs_t playbackPosition = globalContext()->playbackState()->playbackPosition();

    dispatcher()->dispatch(TRACK_SPLIT_AT, ActionData::make_arg2<trackedit::TrackId, secs_t>(trackIdToSplit, playbackPosition));
}

void TrackeditActionsController::doGlobalJoin()
{
    auto selectedTracks = selectionController()->dataSelectedOnTracks();
    secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
    secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

    dispatcher()->dispatch(MERGE_SELECTED_ON_TRACK,
                           ActionData::make_arg3<std::vector<TrackId>, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));
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
    auto selectedTracks = selectionController()->dataSelectedOnTracks();

    if (!selectedTracks.empty()) {
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        dispatcher()->dispatch(DUPLICATE_SELECTED,
                               ActionData::make_arg3<std::vector<TrackId>, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));
    } else {
        ClipKey selectedClipKey = selectionController()->selectedClip();
        if (!selectedClipKey.isValid()) {
            return;
        }
        dispatcher()->dispatch(DUPLICATE_CLIP, ActionData::make_arg1<ClipKey>(selectedClipKey));
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

    secs_t duration = trackeditInteraction()->clipDuration(clipKey);
    secs_t start = trackeditInteraction()->clipStartTime(clipKey);
    trackeditInteraction()->removeClip(clipKey);

    pushProjectHistoryDeleteState(start, duration);
}

void TrackeditActionsController::clipCutSelected()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->dataSelectedOnTracks();
    secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
    secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

    trackeditInteraction()->clearClipboard();
    trackeditInteraction()->cutClipDataIntoClipboard(selectedTracks, selectedStartTime, selectedEndTime);
}

void TrackeditActionsController::clipCopySelected()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->dataSelectedOnTracks();
    secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
    secs_t selectedEndTime = selectionController()->dataSelectedEndTime();
    auto tracks = project->trackeditProject()->trackList();

    trackeditInteraction()->clearClipboard();

    for (const auto& track : tracks) {
        if (std::find(selectedTracks.begin(), selectedTracks.end(), track.id) == selectedTracks.end()) {
            continue;
        }

        trackeditInteraction()->copyTrackDataIntoClipboard(track.id, selectedStartTime, selectedEndTime);
    }
}

void TrackeditActionsController::clipDeleteSelected()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->dataSelectedOnTracks();
    auto selectedStartTime = selectionController()->dataSelectedStartTime();
    auto selectedEndTime = selectionController()->dataSelectedEndTime();
    auto tracks = project->trackeditProject()->trackList();

    secs_t duration = selectedEndTime - selectedStartTime;
    secs_t start = selectedStartTime;

    //! TODO AU4: improve for deleting multiple selected clips

    // remove multiple clips in selected region
    for (const auto& track : tracks) {
        if (std::find(selectedTracks.begin(), selectedTracks.end(), track.id) == selectedTracks.end()) {
            continue;
        }

        auto clips = project->trackeditProject()->clipList(track.id);
        for (const auto& clip: clips) {
            if (selectedStartTime > clip.endTime || selectedEndTime < clip.startTime) {
                continue;
            }

            trackeditInteraction()->removeClipData(clip.key, selectedStartTime, selectedEndTime);
        }
    }

    pushProjectHistoryDeleteState(start, duration);
}

void TrackeditActionsController::paste()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto tracks = project->trackeditProject()->trackList();
    double selectedStartTime = globalContext()->playbackState()->playbackPosition();
    TrackId selectedTrackId = selectionController()->selectedTrack();

    if (!tracks.empty() && selectedStartTime >= 0) {
        trackeditInteraction()->pasteFromClipboard(selectedStartTime, selectedTrackId);

        pushProjectHistoryPasteState();
    }
}

void TrackeditActionsController::trackSplit(const ActionData &args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    TrackId trackIdToSplit = args.arg<TrackId>(0);
    if (trackIdToSplit == -1) {
        return;
    }

    secs_t playbackPosition = globalContext()->playbackState()->playbackPosition();

    dispatcher()->dispatch(TRACK_SPLIT_AT, ActionData::make_arg2<trackedit::TrackId, secs_t>(trackIdToSplit, playbackPosition));
}

void TrackeditActionsController::trackSplitAt(const ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 2) {
        return;
    }

    TrackId trackId = args.arg<TrackId>(0);
    if (trackId == -1) {
        return;
    }

    secs_t playbackPosition = args.arg<secs_t>(1);

    trackeditInteraction()->splitAt(trackId, playbackPosition);
}

void TrackeditActionsController::mergeSelectedOnTrack(const muse::actions::ActionData &args)
{
    IF_ASSERT_FAILED(args.count() == 3) {
        return;
    }

    std::vector<TrackId> tracksIds = args.arg<std::vector<TrackId>>(0);
    if (tracksIds.empty()) {
        return;
    }

    secs_t begin = args.arg<secs_t>(1);
    secs_t end = args.arg<secs_t>(2);
    secs_t duration = end - begin;

    trackeditInteraction()->mergeSelectedOnTracks(tracksIds, begin, end);
}

void TrackeditActionsController::duplicateSelected(const muse::actions::ActionData &args)
{
    IF_ASSERT_FAILED(args.count() == 3) {
        return;
    }

    std::vector<TrackId> tracksIds = args.arg<std::vector<TrackId>>(0);
    if (tracksIds.empty()) {
        return;
    }

    secs_t begin = args.arg<secs_t>(1);
    secs_t end = args.arg<secs_t>(2);

    trackeditInteraction()->duplicateSelectedOnTracks(tracksIds, begin, end);
}

void TrackeditActionsController::duplicateClip(const muse::actions::ActionData &args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    ClipKey clipKey = args.arg<ClipKey>(0);
    trackeditInteraction()->duplicateClip(clipKey);
}

void TrackeditActionsController::clipSplitCut(const muse::actions::ActionData &args)
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

void TrackeditActionsController::clipSplitDelete(const muse::actions::ActionData &args)
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

void TrackeditActionsController::splitCutSelected(const muse::actions::ActionData &args)
{
    IF_ASSERT_FAILED(args.count() == 3) {
        return;
    }

    std::vector<TrackId> tracksIds = args.arg<std::vector<TrackId>>(0);
    if (tracksIds.empty()) {
        return;
    }

    secs_t begin = args.arg<secs_t>(1);
    secs_t end = args.arg<secs_t>(2);

    trackeditInteraction()->clearClipboard();
    trackeditInteraction()->splitCutSelectedOnTracks(tracksIds, begin, end);
}

void TrackeditActionsController::splitDeleteSelected(const muse::actions::ActionData &args)
{
    IF_ASSERT_FAILED(args.count() == 3) {
        return;
    }

    std::vector<TrackId> tracksIds = args.arg<std::vector<TrackId>>(0);
    if (tracksIds.empty()) {
        return;
    }

    secs_t begin = args.arg<secs_t>(1);
    secs_t end = args.arg<secs_t>(2);

    trackeditInteraction()->splitDeleteSelectedOnTracks(tracksIds, begin, end);
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
    pushProjectHistoryTrackAddedState();
}

void TrackeditActionsController::newStereoTrack()
{
    trackeditInteraction()->newStereoTrack();
    pushProjectHistoryTrackAddedState();
}

void TrackeditActionsController::newLabelTrack()
{
    trackeditInteraction()->newLabelTrack();
}

void TrackeditActionsController::trimAudioOutsideSelection()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->dataSelectedOnTracks();
    auto selectedStartTime = selectionController()->dataSelectedStartTime();
    auto selectedEndTime = selectionController()->dataSelectedEndTime();
    auto tracks = project->trackeditProject()->trackList();

    for (const auto& track : tracks) {
        if (std::find(selectedTracks.begin(), selectedTracks.end(), track.id) == selectedTracks.end()) {
            continue;
        }

        trackeditInteraction()->trimTrackData(track.id, selectedStartTime, selectedEndTime);
    }

    pushProjectHistoryTrackTrimState(selectedStartTime, selectedEndTime);
}

void TrackeditActionsController::silenceAudioSelection()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->dataSelectedOnTracks();
    auto selectedStartTime = selectionController()->dataSelectedStartTime();
    auto selectedEndTime = selectionController()->dataSelectedEndTime();
    auto tracks = project->trackeditProject()->trackList();

    for (const auto& track : tracks) {
        if (std::find(selectedTracks.begin(), selectedTracks.end(), track.id) == selectedTracks.end()) {
            continue;
        }

        trackeditInteraction()->silenceTrackData(track.id, selectedStartTime, selectedEndTime);
    }

    pushProjectHistoryTrackSilenceState(selectedStartTime, selectedEndTime);
}

void TrackeditActionsController::pushProjectHistoryTrackAddedState()
{
    projectHistory()->pushHistoryState("Created new audio track", "New track");
}

void TrackeditActionsController::pushProjectHistoryTrackTrimState(secs_t start, secs_t end)
{
    std::stringstream ss;
    ss << "Trim selected audio tracks from " << start << " seconds to " << end << " seconds";

    projectHistory()->pushHistoryState(ss.str(), "Trim Audio");
}

void TrackeditActionsController::pushProjectHistoryTrackSilenceState(secs_t start, secs_t end)
{
    std::stringstream ss;
    ss << "Silenced selected tracks for " << start << " seconds at " << end << "";

    projectHistory()->pushHistoryState(ss.str(), "Silence");
}

void TrackeditActionsController::pushProjectHistoryPasteState()
{
    projectHistory()->pushHistoryState("Pasted from the clipboard", "Paste");
}

void TrackeditActionsController::pushProjectHistoryDeleteState(secs_t start, secs_t duration)
{
    std::stringstream ss;
    ss << "Delete " << duration << " seconds at " << start;

    projectHistory()->pushHistoryState(ss.str(), "Delete");
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

bool TrackeditActionsController::canReceiveAction(const ActionCode&) const
{
    return globalContext()->currentProject() != nullptr;
}
