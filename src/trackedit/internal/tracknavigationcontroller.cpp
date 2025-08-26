/*
* Audacity: A Digital Audio Editor
*/

#include "tracknavigationcontroller.h"

#include "global/containers.h"

using namespace au::trackedit;

static const muse::actions::ActionCode PREV_TRACK_CODE("prev-track");
static const muse::actions::ActionCode NEXT_TRACK_CODE("next-track");
static const muse::actions::ActionCode TRACK_TOGGLE_SELECTION_CODE("track-toggle-focused-selection");

void TrackNavigationController::init()
{
    dispatcher()->reg(this, PREV_TRACK_CODE, this, &TrackNavigationController::navigateUp);
    dispatcher()->reg(this, NEXT_TRACK_CODE, this, &TrackNavigationController::navigateDown);
    dispatcher()->reg(this, TRACK_TOGGLE_SELECTION_CODE, this, &TrackNavigationController::toggleSelectionOnFocusedTrack);
}

void TrackNavigationController::navigateUp()
{
    selectionController()->focusPreviousTrack();
}

void TrackNavigationController::navigateDown()
{
    selectionController()->focusNextTrack();
}

void TrackNavigationController::toggleSelectionOnFocusedTrack()
{
    const au::trackedit::TrackId focusedTrack = selectionController()->focusedTrack();
    au::trackedit::TrackIdList selectedTracks = selectionController()->selectedTracks();

    if (muse::contains(selectedTracks, focusedTrack)) {
        selectedTracks.erase(std::remove(selectedTracks.begin(), selectedTracks.end(), focusedTrack), selectedTracks.end());
    } else {
        selectedTracks.push_back(focusedTrack);
    }

    selectionController()->setSelectedTracks(selectedTracks);
}
