/*
* Audacity: A Digital Audio Editor
*/
#include "trackeditactionscontroller.h"
#include "project/internal/audacityproject.h"

using namespace muse;
using namespace au::trackedit;
using namespace muse::async;
using namespace muse::actions;

static const ActionCode COPY_CODE("copy");
static const ActionCode DELETE_CODE("delete");

static const ActionCode CLIP_CUT_CODE("clip-cut");
static const ActionCode CLIP_COPY_CODE("clip-copy");
static const ActionCode CLIP_DELETE_CODE("clip-delete");
static const ActionCode CLIP_CUT_SELECTED_CODE("clip-cut-selected");
static const ActionCode CLIP_COPY_SELECTED_CODE("clip-copy-selected");
static const ActionCode CLIP_DELETE_SELECTED_CODE("clip-delete-selected");
static const ActionCode PASTE("paste");

void TrackeditActionsController::init()
{
    dispatcher()->reg(this, COPY_CODE, this, &TrackeditActionsController::doGlobalCopy);
    dispatcher()->reg(this, DELETE_CODE, this, &TrackeditActionsController::doGlobalDelete);

    dispatcher()->reg(this, CLIP_CUT_CODE, this, &TrackeditActionsController::clipCut);
    dispatcher()->reg(this, CLIP_COPY_CODE, this, &TrackeditActionsController::clipCopy);
    dispatcher()->reg(this, CLIP_DELETE_CODE, this, &TrackeditActionsController::clipDelete);
    dispatcher()->reg(this, CLIP_CUT_SELECTED_CODE, this, &TrackeditActionsController::clipCutSelected);
    dispatcher()->reg(this, CLIP_COPY_SELECTED_CODE, this, &TrackeditActionsController::clipCopySelected);
    dispatcher()->reg(this, CLIP_DELETE_SELECTED_CODE, this, &TrackeditActionsController::clipDeleteSelected);
    dispatcher()->reg(this, PASTE, this, &TrackeditActionsController::paste);
    dispatcher()->reg(this, "toggle-loop-region", this, &TrackeditActionsController::toggleLoopRegion);
    dispatcher()->reg(this, "clear-loop-region", this, &TrackeditActionsController::clearLoopRegion);
    dispatcher()->reg(this, "set-loop-region-to-selection", this, &TrackeditActionsController::setLoopRegionToSelection);
    dispatcher()->reg(this, "set-selection-to-loop", this, &TrackeditActionsController::setSelectionToLoop);
    dispatcher()->reg(this, "set-loop-region-in", this, &TrackeditActionsController::setLoopRegionIn);
    dispatcher()->reg(this, "set-loop-region-out", this, &TrackeditActionsController::setLoopRegionOut);
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

void TrackeditActionsController::clipCut()
{
    NOT_IMPLEMENTED;
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
    NOT_IMPLEMENTED;
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
        //! TODO AU4: handle multiple tracks copying (need to extend playcursor behaviour first)
        break;
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
        trackeditInteraction()->pasteIntoClipboard(selectedStartTime, selectedTrackId);

        pushProjectHistoryPasteState();
    }
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

void TrackeditActionsController::pushProjectHistoryPasteState()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto trackeditProject = project->trackeditProject();
    trackeditProject->pushHistoryState("Pasted from the clipboard", "Paste");
}

void TrackeditActionsController::pushProjectHistoryDeleteState(secs_t start, secs_t duration)
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto trackeditProject = project->trackeditProject();

    std::stringstream ss;
    ss << "Delete " << duration << " seconds at " << start;

    trackeditProject->pushHistoryState(ss.str(), "Delete");
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
