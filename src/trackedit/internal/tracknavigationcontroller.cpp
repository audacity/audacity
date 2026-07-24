/*
* Audacity: A Digital Audio Editor
*/

#include "tracknavigationcontroller.h"

#include "framework/ui/navigationcommands.h"

#include <algorithm>

#include "framework/global/async/async.h"
#include "framework/global/containers.h"

#include "au3wrap/internal/domaccessor.h"

#include "itrackeditproject.h"
#include "trackedit/trackedittypes.h"

using namespace au::trackedit;

static const muse::actions::ActionCode TRACK_VIEW_NEXT_PANEL_CODE("track-view-next-panel");
static const muse::actions::ActionCode TRACK_VIEW_PREV_PANEL_CODE("track-view-prev-panel");

static const muse::actions::ActionCode TRACK_VIEW_FIRST_TRACK_CODE("track-view-first-track");
static const muse::actions::ActionCode TRACK_VIEW_LAST_TRACK_CODE("track-view-last-track");

static const muse::actions::ActionCode TRACK_VIEW_REPLACE_SELECTION_CODE("track-view-replace-selection");
static const muse::actions::ActionCode TRACK_VIEW_TOGGLE_SELECTION_CODE("track-view-toggle-selection");
static const muse::actions::ActionCode TRACK_VIEW_RANGE_SELECTION_CODE("track-view-range-selection");
static const muse::actions::ActionCode TRACK_VIEW_TRACK_SELECTION_PREV_CODE("track-view-extend-track-selection-prev");
static const muse::actions::ActionCode TRACK_VIEW_TRACK_SELECTION_NEXT_CODE("track-view-extend-track-selection-next");

static const muse::actions::ActionCode TRACK_VIEW_NEXT_ITEM_CODE("track-view-next-item");
static const muse::actions::ActionCode TRACK_VIEW_PREV_ITEM_CODE("track-view-prev-item");
static const muse::actions::ActionCode TRACK_VIEW_ABOVE_ITEM_CODE("track-view-above-item");
static const muse::actions::ActionCode TRACK_VIEW_BELOW_ITEM_CODE("track-view-below-item");

static const muse::actions::ActionCode TRACK_VIEW_ITEM_CONTEXT_MENU_CODE("track-view-item-context-menu");

static const muse::actions::ActionQuery PLAYBACK_SEEK_QUERY("action://playback/seek");

void TrackNavigationController::init()
{
    dispatcher()->reg(this, TRACK_VIEW_NEXT_PANEL_CODE, this, &TrackNavigationController::navigateToNextPanel);
    dispatcher()->reg(this, TRACK_VIEW_PREV_PANEL_CODE, this, &TrackNavigationController::navigateToPrevPanel);

    dispatcher()->reg(this, TRACK_VIEW_FIRST_TRACK_CODE, this, &TrackNavigationController::navigateToFirstTrack);
    dispatcher()->reg(this, TRACK_VIEW_LAST_TRACK_CODE, this, &TrackNavigationController::navigateToLastTrack);

    dispatcher()->reg(this, TRACK_VIEW_NEXT_ITEM_CODE, this, &TrackNavigationController::navigateToNextItem);
    dispatcher()->reg(this, TRACK_VIEW_PREV_ITEM_CODE, this, &TrackNavigationController::navigateToPrevItem);
    dispatcher()->reg(this, TRACK_VIEW_ABOVE_ITEM_CODE, this, &TrackNavigationController::navigateToAboveItem);
    dispatcher()->reg(this, TRACK_VIEW_BELOW_ITEM_CODE, this, &TrackNavigationController::navigateToBelowItem);

    dispatcher()->reg(this, TRACK_VIEW_REPLACE_SELECTION_CODE, this, &TrackNavigationController::replaceSelection);
    dispatcher()->reg(this, TRACK_VIEW_TOGGLE_SELECTION_CODE, this, &TrackNavigationController::toggleSelection);
    dispatcher()->reg(this, TRACK_VIEW_RANGE_SELECTION_CODE, this, &TrackNavigationController::rangeSelection);

    dispatcher()->reg(this, TRACK_VIEW_TRACK_SELECTION_PREV_CODE, this, &TrackNavigationController::multiSelectionUp);
    dispatcher()->reg(this, TRACK_VIEW_TRACK_SELECTION_NEXT_CODE, this, &TrackNavigationController::multiSelectionDown);

    dispatcher()->reg(this, TRACK_VIEW_ITEM_CONTEXT_MENU_CODE, this, &TrackNavigationController::openContextMenuForFocusedItem);

    dispatcher()->reg(this, PLAYBACK_SEEK_QUERY, [this] {
        m_selectionStart = std::nullopt;
    });

    dispatcher()->reg(this, "nav-escape", [this] {
        m_savedItemStartTime = std::nullopt;
        navigationController()->setIsHighlight(false);
    });

    selectionController()->tracksSelected().onReceive(this, [this](const trackedit::TrackIdList& trackIds) {
        if (trackIds.size() == 1) {
            // The idea here is that range selection also supports the base track to be selected using the mouse.
            m_lastSelectedTrack = trackIds.front();
        }
    });

    m_selectionStart = std::nullopt;

    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this]() {
        muse::async::Async::call(this, [this]() {
            ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
            if (prj) {
                std::vector<Track> trackList = prj->trackList();
                if (!trackList.empty()) {
                    setFocusedTrack(trackList.front().id);
                }

                prj->trackAdded().onReceive(this, [this](const Track&) {
                    revalidateFocusedTrack();
                });

                prj->trackInserted().onReceive(this, [this](const Track&, int) {
                    revalidateFocusedTrack();
                });

                prj->trackRemoved().onReceive(this, [this](const Track&) {
                    revalidateFocusedTrack();
                });
            }
        });
    });
}

bool TrackNavigationController::isNavigationEnabled() const
{
    return m_isNavigationActive;
}

void TrackNavigationController::setIsNavigationActive(bool active)
{
    if (m_isNavigationActive == active) {
        return;
    }

    m_isNavigationActive = active;
    m_isNavigationActiveChannel.notify();
}

muse::async::Notification TrackNavigationController::isNavigationActiveChanged() const
{
    return m_isNavigationActiveChannel;
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

void TrackNavigationController::setFocusedItem(const TrackItemKey& itemKey, bool highlight)
{
    TrackItemKey key = itemKey;
    if (key.trackId == INVALID_TRACK) {
        key.trackId = m_focusedItemKey.trackId;
    }

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

TrackItemKeyList TrackNavigationController::itemKeysInRange(const TrackItemKey& anchor, const TrackItemKey& target) const
{
    if (!anchor.isValid() || !target.isValid() || anchor.trackId != target.trackId) {
        return {};
    }

    const TrackItemKeyList ordered = sortedItemsKeys(target.trackId);

    int anchorIndex = -1;
    int targetIndex = -1;
    for (int i = 0; i < static_cast<int>(ordered.size()); ++i) {
        if (ordered.at(i) == anchor) {
            anchorIndex = i;
        }
        if (ordered.at(i) == target) {
            targetIndex = i;
        }
    }

    if (anchorIndex < 0 || targetIndex < 0) {
        return {};
    }

    TrackItemKeyList range;
    for (int i = std::min(anchorIndex, targetIndex); i <= std::max(anchorIndex, targetIndex); ++i) {
        range.push_back(ordered.at(i));
    }

    return range;
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
            commandDispatcher()->dispatch(muse::ui::NEXT_PANEL_COMMAND);
        } else {
            navigateToFirstItem();
        }
        return;
    }

    if (isLastTrack(m_focusedItemKey.trackId)) {
        commandDispatcher()->dispatch(muse::ui::NEXT_PANEL_COMMAND);
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
        commandDispatcher()->dispatch(muse::ui::PREV_PANEL_COMMAND);
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
            if (i == 0) {
                setFocusedTrack(trackList.back().id, true /*highlight*/);
            } else {
                setFocusedTrack(trackList.at(i - 1).id, true /*highlight*/);
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
    if (!navigationController()->isHighlight()) {
        dispatcher()->dispatch("play-position-increase");
        return;
    }

    TrackItemKeyList itemsKeys = sortedItemsKeys(m_focusedItemKey.trackId);

    if (m_focusedItemKey.itemId == INVALID_TRACK_ITEM) {
        commandDispatcher()->dispatch(muse::ui::RIGHT_COMMAND);
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

    m_savedItemStartTime = std::nullopt;
}

void TrackNavigationController::navigateToPrevItem()
{
    if (!navigationController()->isHighlight()) {
        dispatcher()->dispatch("play-position-decrease");
        return;
    }

    TrackItemKeyList itemsKeys = sortedItemsKeys(m_focusedItemKey.trackId);

    if (m_focusedItemKey.itemId == INVALID_TRACK_ITEM) {
        commandDispatcher()->dispatch(muse::ui::LEFT_COMMAND);
        return;
    }

    for (size_t i = 0; i < itemsKeys.size(); ++i) {
        if (itemsKeys[i].itemId == m_focusedItemKey.itemId) {
            if (i == 0) {
                setFocusedItem(itemsKeys.back(), true /*highlight*/);
            } else {
                setFocusedItem(itemsKeys.at(i - 1), true /*highlight*/);
            }
            break;
        }
    }

    m_savedItemStartTime = std::nullopt;
}

double TrackNavigationController::itemStartTime(const TrackItemKey& key) const
{
    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return 0.0;
    }

    std::optional<Track> track = prj->track(key.trackId);
    if (!track.has_value() || track->type == TrackType::Undefined) {
        return 0.0;
    }

    if (track->type == TrackType::Label) {
        Label l = prj->label(key);
        return l.startTime;
    }

    Clip c = prj->clip(key);
    return c.startTime;
}

TrackItemKey TrackNavigationController::findClosestItemOnTrack(const TrackId& trackId, double referenceStartTime) const
{
    TrackItemKeyList itemsKeys = sortedItemsKeys(trackId);
    if (itemsKeys.empty()) {
        return TrackItemKey { trackId, INVALID_TRACK_ITEM };
    }

    TrackItemKey closest = itemsKeys.front();
    double closestDiff = std::numeric_limits<double>::max();

    for (const auto& itemKey : itemsKeys) {
        double diff = std::abs(itemStartTime(itemKey) - referenceStartTime);
        if (diff < closestDiff) {
            closestDiff = diff;
            closest = itemKey;
        }
    }

    return closest;
}

void TrackNavigationController::navigateToAboveItem()
{
    if (m_focusedItemKey.itemId == INVALID_TRACK_ITEM) {
        navigateToPrevTrack();
        return;
    }

    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    std::vector<Track> trackList = prj->trackList();
    if (trackList.empty()) {
        return;
    }

    if (!m_savedItemStartTime.has_value()) {
        m_savedItemStartTime = itemStartTime(m_focusedItemKey);
    }

    const TrackId currentTrackId = m_focusedItemKey.trackId;

    for (size_t i = 0; i < trackList.size(); ++i) {
        if (trackList[i].id == currentTrackId) {
            for (size_t j = i; j > 0; --j) {
                TrackId candidateId = trackList[j - 1].id;
                if (!isTrackItemsEmpty(candidateId)) {
                    TrackItemKey closest = findClosestItemOnTrack(candidateId, *m_savedItemStartTime);
                    setFocusedItem(closest, true /*highlight*/);
                    return;
                }
            }
            return;
        }
    }
}

void TrackNavigationController::navigateToBelowItem()
{
    if (m_focusedItemKey.itemId == INVALID_TRACK_ITEM) {
        navigateToNextTrack();
        return;
    }

    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    std::vector<Track> trackList = prj->trackList();
    if (trackList.empty()) {
        return;
    }

    if (!m_savedItemStartTime.has_value()) {
        m_savedItemStartTime = itemStartTime(m_focusedItemKey);
    }

    const TrackId currentTrackId = m_focusedItemKey.trackId;

    for (size_t i = 0; i < trackList.size(); ++i) {
        if (trackList[i].id == currentTrackId) {
            for (size_t j = i + 1; j < trackList.size(); ++j) {
                TrackId candidateId = trackList[j].id;
                if (!isTrackItemsEmpty(candidateId)) {
                    TrackItemKey closest = findClosestItemOnTrack(candidateId, *m_savedItemStartTime);
                    setFocusedItem(closest, true /*highlight*/);
                    return;
                }
            }
            return;
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

void TrackNavigationController::replaceSelection()
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
    m_lastSelectedItem = (isSelect && !isTrackPanel) ? m_focusedItemKey : TrackItemKey {};
}

void TrackNavigationController::toggleSelection()
{
    bool isTrackPanel = m_focusedItemKey.itemId == INVALID_TRACK_ITEM;

    if (!isTrackPanel) {
        if (isFocusedItemLabel()) {
            LabelKeyList selectedLabels = selectionController()->selectedLabels();
            if (muse::contains(selectedLabels, m_focusedItemKey)) {
                selectionController()->removeLabelSelection(m_focusedItemKey);
            } else {
                selectionController()->addSelectedLabel(m_focusedItemKey);
            }
        } else {
            ClipKeyList selectedClips = selectionController()->selectedClips();
            if (muse::contains(selectedClips, m_focusedItemKey)) {
                selectionController()->removeClipSelection(m_focusedItemKey);
            } else {
                selectionController()->addSelectedClip(m_focusedItemKey);
            }
        }

        m_lastSelectedItem = m_focusedItemKey;
    } else {
        TrackIdList selectedTracks = selectionController()->selectedTracks();
        const TrackId focusedTrack = m_focusedItemKey.trackId;
        if (muse::contains(selectedTracks, focusedTrack)) {
            selectedTracks.erase(std::remove(selectedTracks.begin(), selectedTracks.end(), focusedTrack), selectedTracks.end());
        } else {
            selectedTracks.push_back(focusedTrack);
        }
        selectionController()->setSelectedTracks(selectedTracks);
        m_lastSelectedTrack = focusedTrack;
    }
}

void TrackNavigationController::rangeSelection()
{
    bool isTrackPanel = m_focusedItemKey.itemId == INVALID_TRACK_ITEM;

    bool isSelect = false;

    if (!isTrackPanel) {
        TrackItemKeyList range = itemKeysInRange(m_lastSelectedItem, m_focusedItemKey);
        if (range.empty()) {
            m_lastSelectedItem = m_focusedItemKey;
            range.push_back(m_focusedItemKey);
        }

        if (isFocusedItemLabel()) {
            selectionController()->setSelectedLabels(range);
            selectionController()->setSelectedClips({});
        } else {
            selectionController()->setSelectedClips(range);
            selectionController()->setSelectedLabels({});
        }

        return;
    } else {
        const auto orderedTracks = selectionController()->orderedTrackList();
        if (orderedTracks.empty()) {
            return;
        }

        TrackIdList selectedTracks = selectionController()->selectedTracks();
        TrackId focusedTrack = m_focusedItemKey.trackId;

        if (!m_lastSelectedTrack) {
            m_lastSelectedTrack = focusedTrack;
            selectionController()->setSelectedTracks({ focusedTrack });
            return;
        }

        if (!muse::contains(selectedTracks, *m_lastSelectedTrack)) {
            m_lastSelectedTrack = selectedTracks.size() == 1 ? selectedTracks.front() : focusedTrack;
        }

        auto startIt = std::find(orderedTracks.begin(), orderedTracks.end(), *m_lastSelectedTrack);
        auto endIt = std::find(orderedTracks.begin(), orderedTracks.end(), focusedTrack);

        if (startIt > endIt) {
            std::swap(startIt, endIt);
        }

        au::trackedit::TrackIdList newSelectedTracks;
        for (auto it = startIt; it <= endIt; ++it) {
            newSelectedTracks.push_back(*it);
        }

        selectionController()->setSelectedTracks(newSelectedTracks);
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

void TrackNavigationController::revalidateFocusedTrack()
{
    const TrackId focused = focusedTrack();
    const TrackIdList tracks = selectionController()->orderedTrackList();
    const bool focusedExists = std::any_of(tracks.begin(), tracks.end(),
                                           [focused](const TrackId& t) { return t == focused; });

    if (focusedExists) {
        return;
    }

    const TrackId trackId = tracks.empty() ? INVALID_TRACK : tracks.front();
    setFocusedTrack(trackId);
}
