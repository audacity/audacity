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
static const ActionCode CLIP_COPY_CODE("clip-copy");
static const ActionCode CLIP_DELETE_CODE("clip-delete");
static const ActionCode CLIP_CUT_SELECTED_CODE("clip-cut-selected");
static const ActionCode CLIP_COPY_SELECTED_CODE("clip-copy-selected");
static const ActionCode CLIP_DELETE_SELECTED_CODE("clip-delete-selected");
static const ActionCode CLIP_RENDER_PITCH_AND_SPEED_CODE("clip-render-pitch-speed");
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

static const ActionCode STRETCH_ENABLED_CODE("stretch-clip-to-match-tempo");

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
    dispatcher()->reg(this, CLIP_RENDER_PITCH_AND_SPEED_CODE, this, &TrackeditActionsController::renderClipPitchAndSpeed);
    dispatcher()->reg(this, PASTE, this, &TrackeditActionsController::paste);
    dispatcher()->reg(this, TRACK_SPLIT, this, &TrackeditActionsController::trackSplit);
    dispatcher()->reg(this, TRACK_SPLIT_AT, this, &TrackeditActionsController::tracksSplitAt);
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
    dispatcher()->reg(this, "track-delete", this, &TrackeditActionsController::deleteTracks);
    dispatcher()->reg(this, "track-duplicate", this, &TrackeditActionsController::duplicateTracks);
    dispatcher()->reg(this, "track-move-up", this, &TrackeditActionsController::moveTracksUp);
    dispatcher()->reg(this, "track-move-down", this, &TrackeditActionsController::moveTracksDown);
    dispatcher()->reg(this, "track-move-top", this, &TrackeditActionsController::moveTracksToTop);
    dispatcher()->reg(this, "track-move-bottom", this, &TrackeditActionsController::moveTracksToBottom);

    dispatcher()->reg(this, TRIM_AUDIO_OUTSIDE_SELECTION, this, &TrackeditActionsController::trimAudioOutsideSelection);
    dispatcher()->reg(this, SILENCE_AUDIO_SELECTION, this, &TrackeditActionsController::silenceAudioSelection);

    dispatcher()->reg(this, STRETCH_ENABLED_CODE, this, &TrackeditActionsController::toggleStretchClipToMatchTempo);

    projectHistory()->isUndoRedoAvailableChanged().onNotify(this, [this]() {
        notifyActionEnabledChanged(UNDO);
        notifyActionEnabledChanged(REDO);
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
    if (selectionController()->timeSelectionIsNotEmpty()) {
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
    if (selectionController()->timeSelectionIsNotEmpty()) {
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
    if (selectionController()->timeSelectionIsNotEmpty()) {
        auto selectedTracks = selectionController()->selectedTracks();
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        dispatcher()->dispatch(SPLIT_CUT_SELECTED,
                               ActionData::make_arg3<TrackIdList, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));
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
    if (selectionController()->timeSelectionIsNotEmpty()) {
        auto selectedTracks = selectionController()->selectedTracks();
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        dispatcher()->dispatch(SPLIT_DELETE_SELECTED,
                               ActionData::make_arg3<TrackIdList, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));
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
    TrackIdList tracksIdsToSplit = selectionController()->selectedTracks();

    if (tracksIdsToSplit.empty()) {
        tracksIdsToSplit.push_back(selectionController()->selectedClip().trackId);
    }

    if (tracksIdsToSplit.empty()) {
        return;
    }

    secs_t playbackPosition = globalContext()->playbackState()->playbackPosition();

    dispatcher()->dispatch(TRACK_SPLIT_AT, ActionData::make_arg2<TrackIdList, secs_t>(tracksIdsToSplit, playbackPosition));
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
    auto selectedTracks = selectionController()->selectedTracks();

    if (!selectedTracks.empty()) {
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        dispatcher()->dispatch(DUPLICATE_SELECTED,
                               ActionData::make_arg3<TrackIdList, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));
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

    trackeditInteraction()->removeClip(clipKey);
}

void TrackeditActionsController::clipCutSelected()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->selectedTracks();
    secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
    secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

    trackeditInteraction()->clearClipboard();
    trackeditInteraction()->cutClipDataIntoClipboard(selectedTracks, selectedStartTime, selectedEndTime);

    selectionController()->resetDataSelection();
}

void TrackeditActionsController::clipCopySelected()
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

        trackeditInteraction()->copyTrackDataIntoClipboard(track.id, selectedStartTime, selectedEndTime);
    }
}

void TrackeditActionsController::clipDeleteSelected()
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

void TrackeditActionsController::duplicateClip(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    ClipKey clipKey = args.arg<ClipKey>(0);
    trackeditInteraction()->duplicateClip(clipKey);
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
    } else if (actionCode == UNDO) {
        return trackeditInteraction()->canUndo();
    } else if (actionCode == REDO) {
        return trackeditInteraction()->canRedo();
    }

    return true;
}
