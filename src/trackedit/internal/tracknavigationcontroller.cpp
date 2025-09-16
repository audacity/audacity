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
static const muse::actions::ActionCode MULTI_TRACK_SELECTION_PREV_CODE("shift-up");
static const muse::actions::ActionCode MULTI_TRACK_SELECTION_NEXT_CODE("shift-down");

static const muse::actions::ActionCode PLAYBACK_SEEK_CODE("playback-seek");

void TrackNavigationController::init()
{
    dispatcher()->reg(this, FOCUS_TRACK_INDEX_CODE, this, &TrackNavigationController::focusTrackByIndex);
    dispatcher()->reg(this, FOCUS_PREV_TRACK_CODE, this, &TrackNavigationController::focusPrevTrack);
    dispatcher()->reg(this, FOCUS_NEXT_TRACK_CODE, this, &TrackNavigationController::focusNextTrack);
    dispatcher()->reg(this, PREV_TRACK_CODE, this, &TrackNavigationController::navigateUp);
    dispatcher()->reg(this, NEXT_TRACK_CODE, this, &TrackNavigationController::navigateDown);
    dispatcher()->reg(this, TRACK_TOGGLE_SELECTION_CODE, this, &TrackNavigationController::toggleSelectionOnFocusedTrack);
    dispatcher()->reg(this, MULTI_TRACK_SELECTION_PREV_CODE, this, &TrackNavigationController::multiSelectionUp);
    dispatcher()->reg(this, MULTI_TRACK_SELECTION_NEXT_CODE, this, &TrackNavigationController::multiSelectionDown);

    dispatcher()->reg(this, PLAYBACK_SEEK_CODE, [this] {
        m_selectionStart = std::nullopt;
    });

    selectionController()->focusedTrackChanged().onReceive(this, [this](const trackedit::TrackId& trackId) {
        const auto activePanel = navigationController()->activePanel();
        if (activePanel && activePanel->name() != QString("Track %1 Panel").arg(trackId)) {
            navigationController()->requestActivateByName("Main Section", "Main Panel", "Main Control");
        }
    });

    m_selectionStart = std::nullopt;
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
    m_selectionStart = std::nullopt;
    selectionController()->focusPreviousTrack();
}

void TrackNavigationController::focusNextTrack()
{
    m_selectionStart = std::nullopt;
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

void TrackNavigationController::multiSelectionUp()
{
    updateSelectionStart();

    au::trackedit::TrackIdList selectedTracks = selectionController()->selectedTracks();
    const au::trackedit::TrackId focusedTrack = selectionController()->focusedTrack();

    selectionController()->focusPreviousTrack();
    updateTrackSelection(selectedTracks, focusedTrack);
}

void TrackNavigationController::multiSelectionDown()
{
    updateSelectionStart();

    const au::trackedit::TrackId focusedTrack = selectionController()->focusedTrack();
    au::trackedit::TrackIdList selectedTracks = selectionController()->selectedTracks();

    selectionController()->focusNextTrack();
    updateTrackSelection(selectedTracks, focusedTrack);
}

void TrackNavigationController::updateSelectionStart()
{
    const au::trackedit::TrackId focusedTrack = selectionController()->focusedTrack();

    if (!m_selectionStart) {
        m_selectionStart = focusedTrack;
        selectionController()->setSelectedTracks({ focusedTrack });
    }
}

void TrackNavigationController::updateTrackSelection(TrackIdList& selectedTracks,
                                                     const TrackId& previousFocusedTrack)
{
    const TrackId newFocusedTrack = selectionController()->focusedTrack();
    const int startDistance = selectionController()->trackDistance(*m_selectionStart, previousFocusedTrack);
    const int endDistance = selectionController()->trackDistance(*m_selectionStart, newFocusedTrack);

    if (startDistance == endDistance) {
        return;
    }

    if (std::abs(startDistance) < std::abs(endDistance)) {
        selectedTracks.push_back(newFocusedTrack);
    } else {
        selectedTracks.erase(std::remove(selectedTracks.begin(), selectedTracks.end(), previousFocusedTrack), selectedTracks.end());
    }

    selectionController()->setSelectedTracks(selectedTracks);
}
