/*
* Audacity: A Digital Audio Editor
*/
#include "trackeditactionscontroller.h"
#include "project/internal/audacityproject.h"

using namespace muse;
using namespace au::trackedit;
using namespace muse::async;
using namespace muse::actions;

static const ActionCode CLIP_CUT_CODE("clip-cut");
static const ActionCode CLIP_COPY_CODE("clip-copy");
static const ActionCode CLIP_DELETE_CODE("clip-delete");
static const ActionCode CLIP_CUT_SELECTED_CODE("clip-cut-selected");
static const ActionCode CLIP_COPY_SELECTED_CODE("clip-copy-selected");
static const ActionCode CLIP_DELETE_SELECTED_CODE("clip-delete-selected");
static const ActionCode PASTE("paste");

void TrackeditActionsController::init()
{
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

void TrackeditActionsController::clipCut()
{
    NOT_IMPLEMENTED;
}

void TrackeditActionsController::clipCopy()
{
    NOT_IMPLEMENTED;
}

// called from clips context menu
void TrackeditActionsController::clipDelete()
{
    NOT_IMPLEMENTED;
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
    auto selectedClipKey = selectionController()->selectedClip();

    trackeditInteraction()->clearClipboard();
    // handles single clip selected
    // intentional comparison, see clipId default value
    if (selectedClipKey.clipId != -1) {
        trackeditInteraction()->copyClipIntoClipboard(selectedClipKey);
        return;
    }

    // handles multiple clips selected
    for (const auto& track : tracks) {
        if (std::find(selectedTracks.begin(), selectedTracks.end(), track.id) == selectedTracks.end()) {
            continue;
        }

        trackeditInteraction()->copyTrackDataIntoClipboard(track.id, selectedStartTime, selectedEndTime);
        //! TODO AU4: handle multiple tracks copying (need to extend playcursor behaviour first)
        break;
    }
}

// called from app menu / del shortcut
void TrackeditActionsController::clipDeleteSelected()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->dataSelectedOnTracks();
    auto selectedStartTime = selectionController()->dataSelectedStartTime();
    auto selectedEndTime = selectionController()->dataSelectedEndTime();
    auto tracks = project->trackeditProject()->trackList();
    auto selectedClipKey = selectionController()->selectedClip();

    secs_t duration = selectedEndTime - selectedStartTime;
    secs_t start = selectedStartTime;

    //! TODO AU4: improve for deleting multiple selected clips
    // remove single clip when selected via header click
    if (selectedClipKey.isValid()) {
        duration = trackeditInteraction()->clipDuration(selectedClipKey);
        start = trackeditInteraction()->clipStartTime(selectedClipKey);
        trackeditInteraction()->removeClip(selectedClipKey);
    }

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

    auto trackeditProject = project->trackeditProject();
    std::stringstream ss;
    ss << "Delete " << duration << " seconds at " << start;
    trackeditProject->pushHistoryState(ss.str(), "Delete");
}

void TrackeditActionsController::paste()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto tracks = project->trackeditProject()->trackList();
    double selectedStartTime = globalContext()->playbackState()->playbackPosition();

    if (!tracks.empty() && selectedStartTime >= 0) {
        //! TODO AU4: paste into correct track (need to get info about selected track)
        trackeditInteraction()->pasteIntoClipboard(selectedStartTime, tracks[0].id);
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
