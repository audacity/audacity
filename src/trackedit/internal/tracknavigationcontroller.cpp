/*
* Audacity: A Digital Audio Editor
*/

#include "tracknavigationcontroller.h"

#include "framework/global/containers.h"

#include "trackedit/trackedittypes.h"

using namespace au::trackedit;

static const muse::actions::ActionCode FOCUS_TRACK_INDEX_CODE("focus-track-index");
static const muse::actions::ActionCode FOCUS_PREV_TRACK_CODE("focus-prev-track");
static const muse::actions::ActionCode FOCUS_NEXT_TRACK_CODE("focus-next-track");
static const muse::actions::ActionCode PREV_TRACK_CODE("prev-track");
static const muse::actions::ActionCode NEXT_TRACK_CODE("next-track");
static const muse::actions::ActionCode TRACK_TOGGLE_SELECTION_CODE("track-toggle-focused-selection");
static const muse::actions::ActionCode TRACK_RANGE_SELECTION_CODE("track-range-selection");
static const muse::actions::ActionCode MULTI_TRACK_SELECTION_PREV_CODE("shift-up");
static const muse::actions::ActionCode MULTI_TRACK_SELECTION_NEXT_CODE("shift-down");

static const muse::actions::ActionCode TRACK_VIEW_NEXT_ITEM_CODE("track-view-next-item");
static const muse::actions::ActionCode TRACK_VIEW_PREV_ITEM_CODE("track-view-prev-item");

static const muse::actions::ActionCode TRACK_VIEW_ITEM_MOVE_LEFT_CODE("track-view-item-move-left");
static const muse::actions::ActionCode TRACK_VIEW_ITEM_MOVE_RIGHT_CODE("track-view-item-move-right");
static const muse::actions::ActionCode TRACK_VIEW_ITEM_EXTEND_LEFT_CODE("track-view-item-extend-left");
static const muse::actions::ActionCode TRACK_VIEW_ITEM_EXTEND_RIGHT_CODE("track-view-item-extend-right");
static const muse::actions::ActionCode TRACK_VIEW_ITEM_REDUCE_LEFT_CODE("track-view-item-reduce-left");
static const muse::actions::ActionCode TRACK_VIEW_ITEM_REDUCE_RIGHT_CODE("track-view-item-reduce-right");
static const muse::actions::ActionCode TRACK_VIEW_ITEM_MOVE_UP_CODE("track-view-item-move-up");
static const muse::actions::ActionCode TRACK_VIEW_ITEM_MOVE_DOWN_CODE("track-view-item-move-down");

static const muse::actions::ActionCode TRACK_VIEW_ITEM_CONTEXT_MENU_CODE("track-view-item-context-menu");

static const muse::actions::ActionQuery PLAYBACK_SEEK_QUERY("action://playback/seek");

constexpr double MIN_CLIP_WIDTH = 3.0;

void TrackNavigationController::init()
{
    dispatcher()->reg(this, FOCUS_TRACK_INDEX_CODE, this, &TrackNavigationController::focusTrackByIndex);
    dispatcher()->reg(this, FOCUS_PREV_TRACK_CODE, this, &TrackNavigationController::focusPrevTrack);
    dispatcher()->reg(this, FOCUS_NEXT_TRACK_CODE, this, &TrackNavigationController::focusNextTrack);
    dispatcher()->reg(this, PREV_TRACK_CODE, this, &TrackNavigationController::navigateUp);
    dispatcher()->reg(this, NEXT_TRACK_CODE, this, &TrackNavigationController::navigateDown);
    dispatcher()->reg(this, TRACK_TOGGLE_SELECTION_CODE, this, &TrackNavigationController::toggleSelectionOnFocusedTrack);
    dispatcher()->reg(this, TRACK_RANGE_SELECTION_CODE, this, &TrackNavigationController::trackRangeSelection);
    dispatcher()->reg(this, MULTI_TRACK_SELECTION_PREV_CODE, this, &TrackNavigationController::multiSelectionUp);
    dispatcher()->reg(this, MULTI_TRACK_SELECTION_NEXT_CODE, this, &TrackNavigationController::multiSelectionDown);

    dispatcher()->reg(this, TRACK_VIEW_NEXT_ITEM_CODE, this, &TrackNavigationController::navigateNextItem);
    dispatcher()->reg(this, TRACK_VIEW_PREV_ITEM_CODE, this, &TrackNavigationController::navigatePrevItem);

    dispatcher()->reg(this, TRACK_VIEW_ITEM_MOVE_LEFT_CODE, this, &TrackNavigationController::moveFocusedItemLeft);
    dispatcher()->reg(this, TRACK_VIEW_ITEM_MOVE_RIGHT_CODE, this, &TrackNavigationController::moveFocusedItemRight);
    dispatcher()->reg(this, TRACK_VIEW_ITEM_EXTEND_LEFT_CODE, this, &TrackNavigationController::extendFocusedItemBoundaryLeft);
    dispatcher()->reg(this, TRACK_VIEW_ITEM_EXTEND_RIGHT_CODE, this, &TrackNavigationController::extendFocusedItemBoundaryRight);
    dispatcher()->reg(this, TRACK_VIEW_ITEM_REDUCE_LEFT_CODE, this, &TrackNavigationController::reduceFocusedItemBoundaryLeft);
    dispatcher()->reg(this, TRACK_VIEW_ITEM_REDUCE_RIGHT_CODE, this, &TrackNavigationController::reduceFocusedItemBoundaryRight);
    dispatcher()->reg(this, TRACK_VIEW_ITEM_MOVE_UP_CODE, this, &TrackNavigationController::moveFocusedItemUp);
    dispatcher()->reg(this, TRACK_VIEW_ITEM_MOVE_DOWN_CODE, this, &TrackNavigationController::moveFocusedItemDown);

    dispatcher()->reg(this, TRACK_VIEW_ITEM_CONTEXT_MENU_CODE, this, &TrackNavigationController::openContextMenuForFocusedItem);

    dispatcher()->reg(this, PLAYBACK_SEEK_QUERY, [this] {
        m_selectionStart = std::nullopt;
    });

    focusedTrackChanged().onReceive(this, [this](const trackedit::TrackId& trackId) {
        const auto activePanel = navigationController()->activePanel();
        if (activePanel && activePanel->name() != "AddNewTrackPopup" && activePanel->name() != QString("Track %1 Panel").arg(trackId)) {
            navigationController()->requestActivateByName("Main Section", "Main Panel", "Main Control");
        }
    });

    selectionController()->tracksSelected().onReceive(this, [this](const trackedit::TrackIdList& trackIds) {
        if (trackIds.size() == 1) {
            // The idea here is that range selection also supports the base track to be selected using the mouse.
            m_lastSelectedTrack = trackIds.front();
        }
    });

    m_selectionStart = std::nullopt;

    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this]() {
        ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        if (prj) {
            std::vector<Track> trackList = prj->trackList();
            if (!trackList.empty()) {
                setFocusedTrack(trackList.front().id);
            }
        }
    });
}

au::trackedit::TrackId TrackNavigationController::focusedTrack() const
{
    return m_focusedTrack.val;
}

void TrackNavigationController::setFocusedTrack(TrackId trackId)
{
    m_focusedTrack.set(trackId, true);
}

muse::async::Channel<au::trackedit::TrackId> TrackNavigationController::focusedTrackChanged() const
{
    return m_focusedTrack.changed;
}

void TrackNavigationController::focusTrackByIndex(const muse::actions::ActionData& args)
{
    if (args.count() != 1) {
        return;
    }

    ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    std::vector<Track> trackList = prj->trackList();

    const size_t index = args.arg<int>(0);
    if (index < 0 || index >= trackList.size()) {
        return;
    }

    setFocusedTrack(trackList[index].id);
}

void TrackNavigationController::setFocusedItem(const TrackItemKey& key)
{
    if (m_focusedItemKey == key) {
        return;
    }

    m_focusedItemKey = key;
    m_focusedItemChanged.send(m_focusedItemKey);
}

muse::async::Channel<TrackItemKey> TrackNavigationController::focusedItemChanged() const
{
    return m_focusedItemChanged;
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
    const au::trackedit::TrackId focusedTrackId = focusedTrack();
    au::trackedit::TrackIdList selectedTracks = selectionController()->selectedTracks();

    if (muse::contains(selectedTracks, focusedTrackId)) {
        selectedTracks.erase(std::remove(selectedTracks.begin(), selectedTracks.end(), focusedTrackId), selectedTracks.end());
        m_lastSelectedTrack = std::nullopt;
    } else {
        selectedTracks.push_back(focusedTrackId);
        m_lastSelectedTrack = focusedTrackId;
    }

    selectionController()->setSelectedTracks(selectedTracks);
}

void TrackNavigationController::trackRangeSelection()
{
    const auto orderedTracks = selectionController()->orderedTrackList();
    if (orderedTracks.empty()) {
        return;
    }

    const au::trackedit::TrackId focusedTrackId = focusedTrack();
    const auto selectedTracks = selectionController()->selectedTracks();

    if (!m_lastSelectedTrack) {
        m_lastSelectedTrack = focusedTrackId;
        selectionController()->setSelectedTracks({ focusedTrackId });
        return;
    }

    if (!muse::contains(selectedTracks, *m_lastSelectedTrack)) {
        m_lastSelectedTrack = selectedTracks.size() == 1 ? selectedTracks.front() : focusedTrackId;
    }

    auto startIt = std::find(orderedTracks.begin(), orderedTracks.end(), *m_lastSelectedTrack);
    auto endIt = std::find(orderedTracks.begin(), orderedTracks.end(), focusedTrackId);

    if (startIt > endIt) {
        std::swap(startIt, endIt);
    }

    au::trackedit::TrackIdList newSelectedTracks;
    for (auto it = startIt; it <= endIt; ++it) {
        newSelectedTracks.push_back(*it);
    }

    selectionController()->setSelectedTracks(newSelectedTracks);
}

void TrackNavigationController::multiSelectionUp()
{
    updateSelectionStart(SelectionDirection::Up);

    au::trackedit::TrackIdList selectedTracks = selectionController()->selectedTracks();
    const au::trackedit::TrackId focusedTrackId = focusedTrack();

    focusPrevTrack();
    updateTrackSelection(selectedTracks, focusedTrackId);
}

void TrackNavigationController::multiSelectionDown()
{
    updateSelectionStart(SelectionDirection::Down);

    const au::trackedit::TrackId focusedTrackId = focusedTrack();
    au::trackedit::TrackIdList selectedTracks = selectionController()->selectedTracks();

    focusNextTrack();
    updateTrackSelection(selectedTracks, focusedTrackId);
}

void TrackNavigationController::navigateNextItem()
{
    dispatcher()->dispatch("nav-right");
}

void TrackNavigationController::navigatePrevItem()
{
    dispatcher()->dispatch("nav-left");
}

muse::async::Channel<TrackItemKey> TrackNavigationController::openContextMenuRequested() const
{
    return m_openContextMenuRequested;
}

void TrackNavigationController::updateSelectionStart(SelectionDirection direction)
{
    const au::trackedit::TrackId focusedTrackId = focusedTrack();

    if (!m_selectionStart) {
        const auto orderedTracks = selectionController()->orderedTrackList();
        const auto selectedTracks = selectionController()->selectedTracks();

        std::vector<TrackId> orderedSelectedTracks;
        for (const auto& trackId : orderedTracks) {
            if (muse::contains(selectedTracks, trackId)) {
                orderedSelectedTracks.push_back(trackId);
            }
        }

        if (orderedSelectedTracks.empty()) {
            m_selectionStart = focusedTrackId;
            selectionController()->setSelectedTracks({ focusedTrackId });
            return;
        }

        if (muse::contains(orderedSelectedTracks, focusedTrackId)) {
            const auto& firstTrack = orderedSelectedTracks.front();
            const auto& lastTrack = orderedSelectedTracks.back();

            if (focusedTrackId == firstTrack && direction == SelectionDirection::Down) {
                m_selectionStart = lastTrack;
            } else if (focusedTrackId == lastTrack && direction == SelectionDirection::Up) {
                m_selectionStart = firstTrack;
            } else {
                m_selectionStart = focusedTrackId;
                selectionController()->setSelectedTracks({ focusedTrackId });
            }
        } else {
            m_selectionStart = focusedTrackId;
            selectionController()->setSelectedTracks({ focusedTrackId });
        }
    }
}

void TrackNavigationController::updateTrackSelection(TrackIdList& selectedTracks,
                                                     const TrackId& previousFocusedTrack)
{
    const TrackId newFocusedTrack = focusedTrack();
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

double TrackNavigationController::zoomLevel() const
{
    auto project = globalContext()->currentProject();
    if (!project) {
        return 1.0;
    }

    auto viewState = project->viewState();
    if (!viewState) {
        return 1.0;
    }

    return viewState->zoomState().zoom;
}

double TrackNavigationController::calculateStepSize() const
{
    double zoom = zoomLevel();
    if (zoom <= 0.0) {
        return 1.0;
    }
    return 10.0 / zoom;
}

TrackItemKey TrackNavigationController::focusedItemKey() const
{
    return m_focusedItemKey;
}

bool TrackNavigationController::isFocusedItemValid() const
{
    return m_focusedItemKey.isValid();
}

bool TrackNavigationController::isFocusedItemLabel() const
{
    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return false;
    }

    return prj->track(m_focusedItemKey.trackId)->type == TrackType::Label;
}

Label TrackNavigationController::focusedLabel() const
{
    if (!isFocusedItemValid() || !isFocusedItemLabel()) {
        return {};
    }

    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return {};
    }

    return prj->label({ m_focusedItemKey.trackId, m_focusedItemKey.itemId });
}

au::trackedit::TrackId TrackNavigationController::resolvePreviousTrackIdForMove(const TrackId& trackId) const
{
    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return INVALID_TRACK;
    }

    std::vector<Track> trackList = prj->trackList();
    TrackId lastLabelTrackId = INVALID_TRACK;
    TrackId lastWaveTrackId = INVALID_TRACK;

    for (const Track& track : trackList) {
        bool isLabelTrack = track.type == TrackType::Label;

        if (track.id == trackId) {
            return track.type == TrackType::Label ? lastLabelTrackId : lastWaveTrackId;
        }

        if (isLabelTrack) {
            lastLabelTrackId = track.id;
        } else {
            lastWaveTrackId = track.id;
        }
    }

    return INVALID_TRACK;
}

au::trackedit::TrackId TrackNavigationController::resolveNextTrackIdForMove(const TrackId& trackId) const
{
    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return INVALID_TRACK;
    }

    std::vector<Track> trackList = prj->trackList();

    for (size_t i = 0; i < trackList.size(); ++i) {
        const Track& track = trackList[i];

        if (track.id != trackId) {
            continue;
        }

        bool isLabelTrack = track.type == TrackType::Label;

        while (++i < trackList.size()) {
            const Track& nextTrack = trackList[i];
            if (isLabelTrack && nextTrack.type == TrackType::Label) {
                return nextTrack.id;
            } else if (!isLabelTrack && nextTrack.type != TrackType::Label) {
                return nextTrack.id;
            }
        }
    }

    return INVALID_TRACK;
}

void TrackNavigationController::moveFocusedItemLeft() // todo: not only focused item
{
    const double stepSize = calculateStepSize();
    static bool completed = true;

    if (isFocusedItemLabel()) {
        trackeditInteraction()->moveLabels({ focusedItemKey() }, -stepSize, completed);
    } else {
        static bool itemsMovedToOtherTrack = false;
        trackeditInteraction()->moveClips({ focusedItemKey() }, -stepSize, 0, completed, itemsMovedToOtherTrack);
    }
}

void TrackNavigationController::moveFocusedItemRight()
{
    const double stepSize = calculateStepSize();
    static bool completed = true;

    if (isFocusedItemLabel()) {
        trackeditInteraction()->moveLabels({ focusedItemKey() }, stepSize, completed);
    } else {
        static bool itemsMovedToOtherTrack = false;
        trackeditInteraction()->moveClips({ focusedItemKey() }, stepSize, 0, completed, itemsMovedToOtherTrack);
    }
}

void TrackNavigationController::extendFocusedItemBoundaryLeft()
{
    const double stepSize = calculateStepSize();
    static bool completed = true;

    if (isFocusedItemLabel()) {
        Label label = focusedLabel();
        trackeditInteraction()->stretchLabelLeft(focusedItemKey(), label.startTime - stepSize, completed);
    } else {
        double minClipDuration = MIN_CLIP_WIDTH / zoomLevel();
        trackeditInteraction()->trimClipLeft(focusedItemKey(), -stepSize, minClipDuration, completed, UndoPushType::CONSOLIDATE);
    }
}

void TrackNavigationController::extendFocusedItemBoundaryRight()
{
    const double stepSize = calculateStepSize();
    static bool completed = true;

    if (isFocusedItemLabel()) {
        Label label = focusedLabel();
        trackeditInteraction()->stretchLabelRight(focusedItemKey(), label.endTime + stepSize, completed);
    } else {
        double minClipDuration = MIN_CLIP_WIDTH / zoomLevel();
        trackeditInteraction()->trimClipRight(focusedItemKey(), -stepSize, minClipDuration, completed, UndoPushType::CONSOLIDATE);
    }
}

void TrackNavigationController::reduceFocusedItemBoundaryLeft()
{
    const double stepSize = calculateStepSize();
    static bool completed = true;

    if (isFocusedItemLabel()) {
        Label label = focusedLabel();
        double newStartTime = label.startTime + stepSize;
        if (muse::RealIsEqualOrLess(newStartTime, label.endTime)) {
            trackeditInteraction()->stretchLabelLeft(focusedItemKey(), newStartTime, completed);
        }
    } else {
        double minClipDuration = MIN_CLIP_WIDTH / zoomLevel();
        trackeditInteraction()->trimClipLeft(focusedItemKey(), stepSize, minClipDuration, completed, UndoPushType::CONSOLIDATE);
    }
}

void TrackNavigationController::reduceFocusedItemBoundaryRight()
{
    const double stepSize = calculateStepSize();
    static bool completed = true;

    if (isFocusedItemLabel()) {
        Label label = focusedLabel();
        double newEndTime = label.endTime - stepSize;
        if (muse::RealIsEqualOrMore(newEndTime, label.startTime)) {
            trackeditInteraction()->stretchLabelRight(focusedItemKey(), newEndTime, completed);
        }
    } else {
        double minClipDuration = MIN_CLIP_WIDTH / zoomLevel();
        trackeditInteraction()->trimClipRight(focusedItemKey(), stepSize, minClipDuration, completed, UndoPushType::CONSOLIDATE);
    }
}

void TrackNavigationController::moveFocusedItemUp()
{
    static bool completed = true;

    if (isFocusedItemLabel()) {
        TrackItemKey itemKey = focusedItemKey();
        TrackId previousTrackId = resolvePreviousTrackIdForMove(itemKey.trackId);
        if (previousTrackId != INVALID_TRACK) {
            muse::RetVal<LabelKeyList> result = trackeditInteraction()->moveLabelsToTrack({ itemKey }, previousTrackId, completed);
            if (result.ret) {
                setFocusedItem(result.val.front());
            }
        }
    } else {
        static bool itemsMovedToOtherTrack = false;
        muse::RetVal<ClipKeyList> result
            = trackeditInteraction()->moveClips({ focusedItemKey() }, 0, -1, completed, itemsMovedToOtherTrack);
        if (result.ret) {
            setFocusedItem(result.val.front());
        }
    }
}

void TrackNavigationController::moveFocusedItemDown()
{
    static bool completed = true;

    if (isFocusedItemLabel()) {
        TrackItemKey itemKey = focusedItemKey();
        TrackId nextTrackId = resolveNextTrackIdForMove(itemKey.trackId);
        if (nextTrackId != INVALID_TRACK) {
            muse::RetVal<LabelKeyList> result = trackeditInteraction()->moveLabelsToTrack({ itemKey }, nextTrackId, completed);
            if (result.ret) {
                setFocusedItem(result.val.front());
            }
        }
    } else {
        static bool itemsMovedToOtherTrack = false;
        muse::RetVal<ClipKeyList> result = trackeditInteraction()->moveClips({ focusedItemKey() }, 0, 1, completed, itemsMovedToOtherTrack);
        if (result.ret) {
            setFocusedItem(result.val.front());
        }
    }
}

void TrackNavigationController::openContextMenuForFocusedItem()
{
    if (!isFocusedItemValid()) {
        return;
    }

    m_openContextMenuRequested.send(m_focusedItemKey);
}

void TrackNavigationController::focusPrevTrack()
{
    m_selectionStart = std::nullopt;

    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    std::vector<Track> trackList = prj->trackList();
    const TrackId currentFocusedTrack = focusedTrack();

    for (size_t i = 0; i < trackList.size(); ++i) {
        const Track& track = trackList[i];
        if (track.id == currentFocusedTrack) {
            if (--i >= 0) {
                setFocusedTrack(trackList[i].id);
            }
            return;
        }
    }
}

void TrackNavigationController::focusNextTrack()
{
    m_selectionStart = std::nullopt;

    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    std::vector<Track> trackList = prj->trackList();
    const TrackId currentFocusedTrack = focusedTrack();

    for (size_t i = 0; i < trackList.size(); ++i) {
        const Track& track = trackList[i];
        if (track.id == currentFocusedTrack) {
            if (++i < trackList.size()) {
                setFocusedTrack(trackList[i].id);
            }
            return;
        }
    }
}
