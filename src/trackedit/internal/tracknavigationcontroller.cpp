/*
* Audacity: A Digital Audio Editor
*/

#include "tracknavigationcontroller.h"

#include "framework/global/containers.h"

#include "au3wrap/internal/domaccessor.h"

#include "trackedit/trackedittypes.h"

using namespace au::trackedit;

static const muse::actions::ActionCode TRACK_VIEW_NEXT_PANEL_CODE("track-view-next-panel");
static const muse::actions::ActionCode TRACK_VIEW_PREV_PANEL_CODE("track-view-prev-panel");

static const muse::actions::ActionCode TRACK_VIEW_NEXT_TRACK_CODE("track-view-next-track");
static const muse::actions::ActionCode TRACK_VIEW_PREV_TRACK_CODE("track-view-prev-track");
static const muse::actions::ActionCode TRACK_VIEW_FIRST_TRACK_CODE("track-view-first-track");
static const muse::actions::ActionCode TRACK_VIEW_LAST_TRACK_CODE("track-view-last-track");

static const muse::actions::ActionCode TRACK_VIEW_TOGGLE_SELECTION_CODE("track-view-toggle-selection");
static const muse::actions::ActionCode TRACK_RANGE_SELECTION_CODE("track-view-range-selection");
static const muse::actions::ActionCode TRACK_VIEW_TRACK_SELECTION_PREV_CODE("track-view-extend-track-selection-prev");
static const muse::actions::ActionCode TRACK_VIEW_TRACK_SELECTION_NEXT_CODE("track-view-extend-track-selection-next");

static const muse::actions::ActionCode TRACK_VIEW_NEXT_ITEM_CODE("track-view-next-item");
static const muse::actions::ActionCode TRACK_VIEW_PREV_ITEM_CODE("track-view-prev-item");

static const muse::actions::ActionCode TRACK_VIEW_ITEM_MOVE_LEFT_CODE("track-view-item-move-left");
static const muse::actions::ActionCode TRACK_VIEW_ITEM_MOVE_RIGHT_CODE("track-view-item-move-right");
static const muse::actions::ActionCode TRACK_VIEW_ITEM_MOVE_UP_CODE("track-view-item-move-up");
static const muse::actions::ActionCode TRACK_VIEW_ITEM_MOVE_DOWN_CODE("track-view-item-move-down");
static const muse::actions::ActionCode TRACK_VIEW_ITEM_EXTEND_LEFT_CODE("track-view-item-extend-left");
static const muse::actions::ActionCode TRACK_VIEW_ITEM_EXTEND_RIGHT_CODE("track-view-item-extend-right");
static const muse::actions::ActionCode TRACK_VIEW_ITEM_REDUCE_LEFT_CODE("track-view-item-reduce-left");
static const muse::actions::ActionCode TRACK_VIEW_ITEM_REDUCE_RIGHT_CODE("track-view-item-reduce-right");

static const muse::actions::ActionCode TRACK_VIEW_ITEM_CONTEXT_MENU_CODE("track-view-item-context-menu");

static const muse::actions::ActionQuery PLAYBACK_SEEK_QUERY("action://playback/seek");

constexpr double MIN_CLIP_WIDTH = 3.0;

void TrackNavigationController::init()
{
    dispatcher()->reg(this, TRACK_VIEW_NEXT_PANEL_CODE, this, &TrackNavigationController::navigateToNextPanel);
    dispatcher()->reg(this, TRACK_VIEW_PREV_PANEL_CODE, this, &TrackNavigationController::navigateToPrevPanel);

    dispatcher()->reg(this, TRACK_VIEW_NEXT_TRACK_CODE, this, &TrackNavigationController::navigateToNextTrack);
    dispatcher()->reg(this, TRACK_VIEW_PREV_TRACK_CODE, this, &TrackNavigationController::navigateToPrevTrack);
    dispatcher()->reg(this, TRACK_VIEW_FIRST_TRACK_CODE, this, &TrackNavigationController::navigateToFirstTrack);
    dispatcher()->reg(this, TRACK_VIEW_LAST_TRACK_CODE, this, &TrackNavigationController::navigateToLastTrack);

    dispatcher()->reg(this, TRACK_VIEW_NEXT_ITEM_CODE, this, &TrackNavigationController::navigateToNextItem);
    dispatcher()->reg(this, TRACK_VIEW_PREV_ITEM_CODE, this, &TrackNavigationController::navigateToPrevItem);

    dispatcher()->reg(this, TRACK_VIEW_ITEM_MOVE_LEFT_CODE, this, &TrackNavigationController::moveFocusedItemLeft);
    dispatcher()->reg(this, TRACK_VIEW_ITEM_MOVE_RIGHT_CODE, this, &TrackNavigationController::moveFocusedItemRight);
    dispatcher()->reg(this, TRACK_VIEW_ITEM_MOVE_UP_CODE, this, &TrackNavigationController::moveFocusedItemUp);
    dispatcher()->reg(this, TRACK_VIEW_ITEM_MOVE_DOWN_CODE, this, &TrackNavigationController::moveFocusedItemDown);
    dispatcher()->reg(this, TRACK_VIEW_ITEM_EXTEND_LEFT_CODE, this, &TrackNavigationController::extendFocusedItemBoundaryLeft);
    dispatcher()->reg(this, TRACK_VIEW_ITEM_EXTEND_RIGHT_CODE, this, &TrackNavigationController::extendFocusedItemBoundaryRight);
    dispatcher()->reg(this, TRACK_VIEW_ITEM_REDUCE_LEFT_CODE, this, &TrackNavigationController::reduceFocusedItemBoundaryLeft);
    dispatcher()->reg(this, TRACK_VIEW_ITEM_REDUCE_RIGHT_CODE, this, &TrackNavigationController::reduceFocusedItemBoundaryRight);

    dispatcher()->reg(this, TRACK_VIEW_TOGGLE_SELECTION_CODE, this, &TrackNavigationController::toggleSelection);
    dispatcher()->reg(this, TRACK_RANGE_SELECTION_CODE, this, &TrackNavigationController::trackRangeSelection);

    dispatcher()->reg(this, TRACK_VIEW_TRACK_SELECTION_PREV_CODE, this, &TrackNavigationController::multiSelectionUp);
    dispatcher()->reg(this, TRACK_VIEW_TRACK_SELECTION_NEXT_CODE, this, &TrackNavigationController::multiSelectionDown);

    dispatcher()->reg(this, TRACK_VIEW_ITEM_CONTEXT_MENU_CODE, this, &TrackNavigationController::openContextMenuForFocusedItem);

    dispatcher()->reg(this, PLAYBACK_SEEK_QUERY, [this] {
        m_selectionStart = std::nullopt;
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
    return m_focusedItemKey.trackId;
}

void TrackNavigationController::setFocusedTrack(const TrackId& trackId, bool highlight)
{
    TrackItemKey newKey;

    newKey.trackId = trackId;
    newKey.itemId = INVALID_TRACK_ITEM;

    if (m_focusedItemKey == newKey) {
        return;
    }

    m_focusedItemKey = newKey;

    au3SetTrackFocused(m_focusedItemKey.trackId);

    m_focusedTrackChanged.send(m_focusedItemKey.trackId, highlight);
}

TrackItemKey TrackNavigationController::focusedItem() const
{
    return m_focusedItemKey;
}

muse::async::Channel<au::trackedit::TrackId, bool> TrackNavigationController::focusedTrackChanged() const
{
    return m_focusedTrackChanged;
}

void TrackNavigationController::setFocusedItem(const TrackItemKey& key, bool highlight)
{
    if (m_focusedItemKey == key) {
        return;
    }

    bool isTrackChanged = m_focusedItemKey.trackId != key.trackId;

    m_focusedItemKey = key;

    if (isTrackChanged) {
        au3SetTrackFocused(m_focusedItemKey.trackId);

        m_focusedTrackChanged.send(key.trackId, highlight);
    }

    m_focusedItemChanged.send(key, highlight);
}

muse::async::Channel<TrackItemKey, bool> TrackNavigationController::focusedItemChanged() const
{
    return m_focusedItemChanged;
}

muse::async::Channel<TrackItemKey> TrackNavigationController::openContextMenuRequested() const
{
    return m_openContextMenuRequested;
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

TrackItemKeyList TrackNavigationController::sortedItemsKeys(const TrackId& trackId) const
{
    TrackItemKeyList result;

    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return result;
    }

    std::optional<Track> track = prj->track(trackId);
    if (!track.has_value()) {
        return result;
    }

    if (track->type == TrackType::Label) {
        auto labelList = prj->labelList(track->id);
        std::sort(labelList.begin(), labelList.end(), [](const Label& a, const Label& b){
            return a.startTime < b.startTime;
        });

        for (auto& label : labelList) {
            result.emplace_back(label.key);
        }
    } else {
        auto clipList = prj->clipList(track->id);
        std::sort(clipList.begin(), clipList.end(), [](const Clip& a, const Clip& b){
            return a.startTime < b.startTime;
        });

        for (auto& clip : clipList) {
            result.emplace_back(clip.key);
        }
    }

    return result;
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

bool TrackNavigationController::isTrackItemsEmpty(const TrackId& trackId) const
{
    TrackItemKeyList itemsKeys = sortedItemsKeys(trackId);
    return itemsKeys.empty();
}

bool TrackNavigationController::isFirstTrack(const TrackId& trackId) const
{
    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return true;
    }

    std::vector<Track> trackList = prj->trackList();
    if (trackList.empty()) {
        return true;
    }

    return trackList.front().id == trackId;
}

bool TrackNavigationController::isLastTrack(const TrackId& trackId) const
{
    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return true;
    }

    std::vector<Track> trackList = prj->trackList();
    if (trackList.empty()) {
        return true;
    }

    return trackList.back().id == trackId;
}

void TrackNavigationController::navigateToNextPanel()
{
    bool isTrackPanel = m_focusedItemKey.itemId == INVALID_TRACK_ITEM;

    if (isTrackPanel) {
        if (isTrackItemsEmpty(m_focusedItemKey.trackId)) {
            dispatcher()->dispatch("nav-next-panel");
        } else {
            navigateToFirstItem();
        }
        return;
    }

    if (isLastTrack(m_focusedItemKey.trackId)) {
        dispatcher()->dispatch("nav-next-panel");
        return;
    }

    navigateToNextTrack();
}

void TrackNavigationController::navigateToPrevPanel()
{
    bool isTrackPanel = m_focusedItemKey.itemId == INVALID_TRACK_ITEM;

    if (!isTrackPanel) {
        setFocusedTrack(m_focusedItemKey.trackId, true /*highlight*/);
        return;
    }

    m_focusedItemKey.itemId = INVALID_TRACK_ITEM;

    if (isFirstTrack(m_focusedItemKey.trackId)) {
        dispatcher()->dispatch("nav-prev-panel");
        return;
    }

    navigateToPrevTrack();

    if (!isTrackItemsEmpty(m_focusedItemKey.trackId)) {
        navigateToLastItem();
    }
}

void TrackNavigationController::navigateToPrevTrack()
{
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
                setFocusedTrack(trackList[i].id, true /*highlight*/);
            } else {
                setFocusedTrack(trackList.back().id, true /*highlight*/);
            }
            return;
        }
    }
}

void TrackNavigationController::navigateToNextTrack()
{
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
                setFocusedTrack(trackList[i].id, true /*highlight*/);
            } else {
                setFocusedTrack(trackList.front().id, true /*highlight*/);
            }
            return;
        }
    }
}

void TrackNavigationController::navigateToFirstTrack()
{
    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    std::vector<Track> trackList = prj->trackList();
    if (!trackList.empty()) {
        setFocusedTrack(trackList.front().id, true /*highlight*/);
    }
}

void TrackNavigationController::navigateToLastTrack()
{
    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    std::vector<Track> trackList = prj->trackList();
    if (!trackList.empty()) {
        setFocusedTrack(trackList.back().id, true /*highlight*/);
    }
}

void TrackNavigationController::navigateToNextItem()
{
    TrackItemKeyList itemsKeys = sortedItemsKeys(m_focusedItemKey.trackId);

    if (m_focusedItemKey.itemId == INVALID_TRACK_ITEM) {
        dispatcher()->dispatch("nav-right");
        return;
    }

    for (size_t i = 0; i < itemsKeys.size(); ++i) {
        if (itemsKeys[i].itemId == m_focusedItemKey.itemId) {
            if (++i >= itemsKeys.size()) {
                setFocusedItem(itemsKeys.front(), true /*highlight*/);
            } else {
                setFocusedItem(itemsKeys[i], true /*highlight*/);
            }
            break;
        }
    }
}

void TrackNavigationController::navigateToPrevItem()
{
    TrackItemKeyList itemsKeys = sortedItemsKeys(m_focusedItemKey.trackId);

    if (m_focusedItemKey.itemId == INVALID_TRACK_ITEM) {
        dispatcher()->dispatch("nav-left");
        return;
    }

    for (size_t i = 0; i < itemsKeys.size(); ++i) {
        if (itemsKeys[i].itemId == m_focusedItemKey.itemId) {
            if (i == 0) {
                setFocusedItem(itemsKeys.back(), true /*highlight*/);
            } else {
                setFocusedItem(itemsKeys[i - 1], true /*highlight*/);
            }
            break;
        }
    }
}

void TrackNavigationController::navigateToFirstItem()
{
    TrackItemKeyList itemsKeys = sortedItemsKeys(m_focusedItemKey.trackId);

    if (itemsKeys.empty()) {
        navigateToNextTrack();
        return;
    }

    setFocusedItem(itemsKeys.front(), true /*highlight*/);
}

void TrackNavigationController::navigateToLastItem()
{
    TrackItemKeyList itemsKeys = sortedItemsKeys(m_focusedItemKey.trackId);

    if (itemsKeys.empty()) {
        navigateToPrevTrack();
        return;
    }

    setFocusedItem(itemsKeys.back(), true /*highlight*/);
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

void TrackNavigationController::moveFocusedItemUp()
{
    static bool completed = true;

    if (isFocusedItemLabel()) {
        TrackItemKey itemKey = focusedItemKey();
        TrackId previousTrackId = resolvePreviousTrackIdForMove(itemKey.trackId);
        if (previousTrackId != INVALID_TRACK) {
            muse::RetVal<LabelKeyList> result = trackeditInteraction()->moveLabelsToTrack({ itemKey }, previousTrackId, completed);
            if (result.ret) {
                setFocusedItem(result.val.front(), true /*highlight*/);
            }
        }
    } else {
        static bool itemsMovedToOtherTrack = false;
        muse::RetVal<ClipKeyList> result
            = trackeditInteraction()->moveClips({ focusedItemKey() }, 0, -1, completed, itemsMovedToOtherTrack);
        if (result.ret) {
            setFocusedItem(result.val.front(), true /*highlight*/);
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
                setFocusedItem(result.val.front(), true /*highlight*/);
            }
        }
    } else {
        static bool itemsMovedToOtherTrack = false;
        muse::RetVal<ClipKeyList> result = trackeditInteraction()->moveClips({ focusedItemKey() }, 0, 1, completed, itemsMovedToOtherTrack);
        if (result.ret) {
            setFocusedItem(result.val.front(), true /*highlight*/);
        }
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

void TrackNavigationController::toggleSelection()
{
    bool isTrackPanel = m_focusedItemKey.itemId == INVALID_TRACK_ITEM;

    bool isSelect = false;

    if (!isTrackPanel) {
        if (isFocusedItemLabel()) {
            LabelKeyList selectedLabels = selectionController()->selectedLabels();
            isSelect = !muse::contains(selectedLabels, m_focusedItemKey);
            selectionController()->setSelectedLabels(isSelect ? LabelKeyList { m_focusedItemKey } : LabelKeyList {});

            //! reset clips
            selectionController()->setSelectedClips({ });
        } else {
            ClipKeyList selectedClips = selectionController()->selectedClips();
            isSelect = !muse::contains(selectedClips, m_focusedItemKey);
            selectionController()->setSelectedClips(isSelect ? ClipKeyList { m_focusedItemKey } : ClipKeyList {});

            //! reset labels
            selectionController()->setSelectedLabels({});
        }
    } else {
        TrackIdList selectedTracks = selectionController()->selectedTracks();
        isSelect = !muse::contains(selectedTracks, m_focusedItemKey.trackId);
        selectionController()->setSelectedTracks(isSelect ? TrackIdList { m_focusedItemKey.trackId } : TrackIdList {});
    }

    m_lastSelectedTrack = isSelect ? std::optional<TrackId>(m_focusedItemKey.trackId) : std::nullopt;
}

void TrackNavigationController::trackRangeSelection()
{
    bool isTrackPanel = m_focusedItemKey.itemId == INVALID_TRACK_ITEM;

    bool isSelect = false;

    if (!isTrackPanel) {
        if (isFocusedItemLabel()) {
            LabelKeyList selectedLabels = selectionController()->selectedLabels();
            isSelect = !muse::contains(selectedLabels, m_focusedItemKey);
            selectionController()->addSelectedLabel(m_focusedItemKey);
        } else {
            ClipKeyList selectedClips = selectionController()->selectedClips();
            isSelect = !muse::contains(selectedClips, m_focusedItemKey);
            selectionController()->addSelectedClip(m_focusedItemKey);
        }
    } else {
        TrackIdList selectedTracks = selectionController()->selectedTracks();
        if (muse::contains(selectedTracks, m_focusedItemKey.trackId)) {
            selectedTracks.erase(std::remove(selectedTracks.begin(), selectedTracks.end(), m_focusedItemKey.trackId), selectedTracks.end());
            isSelect = false;
        } else {
            selectedTracks.push_back(m_focusedItemKey.trackId);
            isSelect = true;
        }
        selectionController()->setSelectedTracks(selectedTracks);
    }

    m_lastSelectedTrack = isSelect ? std::optional<TrackId>(m_focusedItemKey.trackId) : std::nullopt;
}

void TrackNavigationController::multiSelectionUp()
{
    updateSelectionStart(SelectionDirection::Up);

    au::trackedit::TrackIdList selectedTracks = selectionController()->selectedTracks();
    const au::trackedit::TrackId focusedTrackId = focusedTrack();

    navigateToPrevTrack();
    updateTrackSelection(selectedTracks, focusedTrackId);
}

void TrackNavigationController::multiSelectionDown()
{
    updateSelectionStart(SelectionDirection::Down);

    const au::trackedit::TrackId focusedTrackId = focusedTrack();
    au::trackedit::TrackIdList selectedTracks = selectionController()->selectedTracks();

    navigateToNextTrack();
    updateTrackSelection(selectedTracks, focusedTrackId);
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

void TrackNavigationController::openContextMenuForFocusedItem()
{
    if (!isFocusedItemValid()) {
        return;
    }

    m_openContextMenuRequested.send(m_focusedItemKey);
}

void TrackNavigationController::au3SetTrackFocused(const TrackId& trackId)
{
    if (auto project = globalContext()->currentProject()) {
        auto au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
        au3::DomAccessor::clearAllTrackFocus(*au3Project);
        au3::DomAccessor::setTrackFocused(*au3Project, trackId, true);
    }
}
