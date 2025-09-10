/*
* Audacity: A Digital Audio Editor
*/

#include "tracknavigationcontroller.h"

#include "global/containers.h"

using namespace au::trackedit;

static const muse::actions::ActionCode FOCUS_TRACK_INDEX_CODE("focus-track-index");
static const muse::actions::ActionCode FOCUS_PREV_TRACK_CODE("focus-prev-track");
static const muse::actions::ActionCode FOCUS_NEXT_TRACK_CODE("focus-next-track");
static const muse::actions::ActionCode PREV_TRACK_CODE("prev-track");
static const muse::actions::ActionCode NEXT_TRACK_CODE("next-track");
static const muse::actions::ActionCode TRACK_TOGGLE_SELECTION_CODE("track-toggle-focused-selection");

void TrackNavigationController::init()
{
    dispatcher()->reg(this, FOCUS_TRACK_INDEX_CODE, this, &TrackNavigationController::focusTrackByIndex);
    dispatcher()->reg(this, FOCUS_PREV_TRACK_CODE, this, &TrackNavigationController::focusPrevTrack);
    dispatcher()->reg(this, FOCUS_NEXT_TRACK_CODE, this, &TrackNavigationController::focusNextTrack);
    dispatcher()->reg(this, PREV_TRACK_CODE, this, &TrackNavigationController::navigateUp);
    dispatcher()->reg(this, NEXT_TRACK_CODE, this, &TrackNavigationController::navigateDown);
    dispatcher()->reg(this, TRACK_TOGGLE_SELECTION_CODE, this, &TrackNavigationController::toggleSelectionOnFocusedTrack);

    selectionController()->focusedTrackChanged().onReceive(this, [this](const trackedit::TrackId& trackId) {
        const auto activePanel = navigationController()->activePanel();
        if (activePanel && activePanel->name() != QString("Track %1 Panel").arg(trackId)) {
            navigationController()->requestActivateByName("Main Section", "Main Panel", "Main Control");
        }
    });
}

void TrackNavigationController::focusTrackByIndex(const muse::actions::ActionData& args)
{
    if (args.count() != 1) {
        return;
    }

    const int index = args.arg<int>(0);
    if (index < 0) {
        return;
    }

    selectionController()->focusTrackByIndex(index);
}

void TrackNavigationController::focusPrevTrack()
{
    selectionController()->focusPreviousTrack();
}

void TrackNavigationController::focusNextTrack()
{
    selectionController()->focusNextTrack();
}

void TrackNavigationController::navigateUp(const muse::actions::ActionData& args)
{
    if (args.count() != 1) {
        return;
    }

    const auto section = navigationController()->activeSection();
    if (!section) {
        return;
    }

    const int position = args.arg<int>(0) - 1;
    if (position < 0) {
        return;
    }

    const auto panels = section->panels();
    if (static_cast<size_t>(2 * position) >= panels.size()) {
        return;
    }

    const auto& panel = std::find_if(panels.begin(), panels.end(), [position](const muse::ui::INavigationPanel* p) {
        return p->index().order() == 2 * position;
    });

    if (panel == panels.end()) {
        return;
    }

    const auto firstControl = (*panel)->controls().begin();
    if (!(*firstControl)) {
        return;
    }

    navigationController()->setIsResetOnMousePress(false);
    navigationController()->setIsHighlight(true);
    navigationController()->requestActivateByName(section->name().toStdString(),
                                                  (*panel)->name().toStdString(), (*firstControl)->name().toStdString());
}

void TrackNavigationController::navigateDown(const muse::actions::ActionData& args)
{
    if (args.count() != 1) {
        return;
    }

    const auto section = navigationController()->activeSection();
    if (!section) {
        return;
    }

    const int position = args.arg<int>(0) + 1;
    const auto panels = section->panels();
    if (static_cast<size_t>(2 * position) >= panels.size()) {
        return;
    }

    const auto& panel = std::find_if(panels.begin(), panels.end(), [position](const muse::ui::INavigationPanel* p) {
        return p->index().order() == 2 * position;
    });

    if (panel == panels.end()) {
        return;
    }

    const auto firstControl = (*panel)->controls().begin();
    if (!(*firstControl)) {
        return;
    }

    navigationController()->setIsResetOnMousePress(false);
    navigationController()->setIsHighlight(true);
    navigationController()->requestActivateByName(section->name().toStdString(),
                                                  (*panel)->name().toStdString(), (*firstControl)->name().toStdString());
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
