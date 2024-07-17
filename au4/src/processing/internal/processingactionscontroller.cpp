/*
* Audacity: A Digital Audio Editor
*/
#include "processingactionscontroller.h"
#include "project/internal/audacityproject.h"

using namespace muse;
using namespace au::processing;
using namespace muse::async;
using namespace muse::actions;

static const ActionCode CLIP_CUT_CODE("clip-cut");
static const ActionCode CLIP_COPY_CODE("clip-copy");
static const ActionCode CLIP_DELETE_CODE("clip-delete");
static const ActionCode CLIP_DELETE_SELECTED_CODE("clip-delete-selected");

void ProcessingActionsController::init()
{
    dispatcher()->reg(this, CLIP_CUT_CODE, this, &ProcessingActionsController::clipCut);
    dispatcher()->reg(this, CLIP_COPY_CODE, this, &ProcessingActionsController::clipCopy);
    dispatcher()->reg(this, CLIP_DELETE_CODE, this, &ProcessingActionsController::clipDelete);
    dispatcher()->reg(this, CLIP_DELETE_SELECTED_CODE, this, &ProcessingActionsController::clipDeleteSelected);
    dispatcher()->reg(this, "toggle-loop-region", this, &ProcessingActionsController::toggleLoopRegion);
    dispatcher()->reg(this, "clear-loop-region", this, &ProcessingActionsController::clearLoopRegion);
    dispatcher()->reg(this, "set-loop-region-to-selection", this, &ProcessingActionsController::setLoopRegionToSelection);
    dispatcher()->reg(this, "set-selection-to-loop", this, &ProcessingActionsController::setSelectionToLoop);
    dispatcher()->reg(this, "set-loop-region-in", this, &ProcessingActionsController::setLoopRegionIn);
    dispatcher()->reg(this, "set-loop-region-out", this, &ProcessingActionsController::setLoopRegionOut);
}

void ProcessingActionsController::notifyActionCheckedChanged(const ActionCode& actionCode)
{
    m_actionCheckedChanged.send(actionCode);
}

void ProcessingActionsController::clipCut()
{
    NOT_IMPLEMENTED;
}

void ProcessingActionsController::clipCopy()
{
    NOT_IMPLEMENTED;
}

// called from clips context menu
void ProcessingActionsController::clipDelete()
{
    NOT_IMPLEMENTED;
}

// called from app menu / del shortcut
void ProcessingActionsController::clipDeleteSelected()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->dataSelectedOnTracks();
    auto selectedStartTime = selectionController()->dataSelectedStartTime();
    auto selectedEndTime = selectionController()->dataSelectedEndTime();
    auto tracks = project->processingProject()->trackList();
    auto selectedClipKey = selectionController()->selectedClip();

    secs_t duration = selectedEndTime - selectedStartTime;
    secs_t start = selectedStartTime;

    //! TODO AU4: improve for deleting multiple selected clips
    // remove single clip when selected via header click
    if (selectedClipKey.id != 0) {
        duration = processingInteraction()->clipDuration(selectedClipKey);
        start = processingInteraction()->clipStartTime(selectedClipKey);
        processingInteraction()->removeClip(selectedClipKey);
    }

    // remove multiple clips in selected region
    for (const auto& track : tracks) {
        if (std::find(selectedTracks.begin(), selectedTracks.end(), track.id) == selectedTracks.end()) {
            continue;
        }

        auto clips = project->processingProject()->clipList(track.id);
        for (const auto& clip: clips) {
            if (selectedStartTime > clip.endTime || selectedEndTime < clip.startTime) {
                continue;
            }

            processingInteraction()->removeClipData(clip.key, selectedStartTime, selectedEndTime);
        }
    }

    auto processingProject = project->processingProject();
    std::stringstream ss;
    ss << "Delete " << duration << " seconds at " << start;
    processingProject->pushHistoryState(ss.str(), "Delete");
}

void ProcessingActionsController::toggleLoopRegion()
{
    NOT_IMPLEMENTED;
}

void ProcessingActionsController::clearLoopRegion()
{
    NOT_IMPLEMENTED;
}

void ProcessingActionsController::setLoopRegionToSelection()
{
    NOT_IMPLEMENTED;
}

void ProcessingActionsController::setSelectionToLoop()
{
    NOT_IMPLEMENTED;
}

void ProcessingActionsController::setLoopRegionIn()
{
    NOT_IMPLEMENTED;
}

void ProcessingActionsController::setLoopRegionOut()
{
    NOT_IMPLEMENTED;
}

bool ProcessingActionsController::actionChecked(const ActionCode& actionCode) const
{
    //! TODO AU4
    return false;
}

Channel<ActionCode> ProcessingActionsController::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}

bool ProcessingActionsController::canReceiveAction(const ActionCode&) const
{
    return globalContext()->currentProject() != nullptr;
}
