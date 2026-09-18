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

#include "log.h"

// #define TRACK_NAVIGATION_LOGGING_ENABLED

#ifdef TRACK_NAVIGATION_LOGGING_ENABLED
#define MYLOG() LOGI()
#else
#define MYLOG() LOGN()
#endif

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

static const muse::actions::ActionCode TRACK_VIEW_ABOVE_ITEM_CODE("track-view-above-item");
static const muse::actions::ActionCode TRACK_VIEW_BELOW_ITEM_CODE("track-view-below-item");

static const muse::actions::ActionCode TRACK_VIEW_ITEM_CONTEXT_MENU_CODE("track-view-item-context-menu");
static const muse::actions::ActionCode TRACK_VIEW_RULER_CONTEXT_MENU_CODE("track-view-ruler-context-menu");

static const muse::actions::ActionQuery PLAYBACK_SEEK_QUERY("action://playback/seek");

void TrackNavigationController::init()
{
    dispatcher()->reg(this, TRACK_VIEW_NEXT_PANEL_CODE, this, &TrackNavigationController::navigateToNextPanel);
    dispatcher()->reg(this, TRACK_VIEW_PREV_PANEL_CODE, this, &TrackNavigationController::navigateToPrevPanel);

    dispatcher()->reg(this, TRACK_VIEW_FIRST_TRACK_CODE, this, &TrackNavigationController::navigateToFirstTrack);
    dispatcher()->reg(this, TRACK_VIEW_LAST_TRACK_CODE, this, &TrackNavigationController::navigateToLastTrack);

    dispatcher()->reg(this, TRACK_VIEW_ABOVE_ITEM_CODE, this, &TrackNavigationController::navigateToAboveItem);
    dispatcher()->reg(this, TRACK_VIEW_BELOW_ITEM_CODE, this, &TrackNavigationController::navigateToBelowItem);

    dispatcher()->reg(this, TRACK_VIEW_REPLACE_SELECTION_CODE, this, &TrackNavigationController::replaceSelection);
    dispatcher()->reg(this, TRACK_VIEW_TOGGLE_SELECTION_CODE, this, &TrackNavigationController::toggleSelection);
    dispatcher()->reg(this, TRACK_VIEW_RANGE_SELECTION_CODE, this, &TrackNavigationController::rangeSelection);

    dispatcher()->reg(this, TRACK_VIEW_TRACK_SELECTION_PREV_CODE, this, &TrackNavigationController::multiSelectionUp);
    dispatcher()->reg(this, TRACK_VIEW_TRACK_SELECTION_NEXT_CODE, this, &TrackNavigationController::multiSelectionDown);

    dispatcher()->reg(this, TRACK_VIEW_ITEM_CONTEXT_MENU_CODE, this, &TrackNavigationController::openContextMenuForFocusedItem);
    dispatcher()->reg(this, TRACK_VIEW_RULER_CONTEXT_MENU_CODE, this, &TrackNavigationController::openContextMenuForFocusedRuler);

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
        muse::async::Async::call(this, [this]() {
            ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
            if (prj) {
                std::vector<Track> trackList = prj->trackList();
                if (!trackList.empty()) {
                    setFocus(TrackFocus::track(trackList.front().id));
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

    MYLOG() << "navigation active: " << active;

    m_isNavigationActive = active;
    m_isNavigationActiveChannel.notify();
}

muse::async::Notification TrackNavigationController::isNavigationActiveChanged() const
{
    return m_isNavigationActiveChannel;
}

au::trackedit::TrackId TrackNavigationController::focusedTrack() const
{
    return m_focus.trackId;
}

muse::async::Channel<au::trackedit::TrackId, bool> TrackNavigationController::focusedTrackChanged() const
{
    return m_focusedTrackChanged;
}

TrackFocus TrackNavigationController::focus() const
{
    return m_focus;
}

void TrackNavigationController::setFocus(const TrackFocus& focus, bool highlight)
{
    if (m_focus == focus) {
        return;
    }

    const bool isTrackChanged = m_focus.trackId != focus.trackId;

    MYLOG() << "track: " << focus.trackId << ", item: " << focusedItemKey().itemId << ", highlight: " << highlight
            << ", track changed: " << isTrackChanged;

    m_focus = focus;

    if (isTrackChanged) {
        au3SetTrackFocused(m_focus.trackId);

        m_focusedTrackChanged.send(m_focus.trackId, highlight);
    }

    m_focusChanged.send(m_focus, highlight);
}

muse::async::Channel<TrackFocus, bool> TrackNavigationController::focusChanged() const
{
    return m_focusChanged;
}

muse::async::Channel<TrackItemKey> TrackNavigationController::openContextMenuRequested() const
{
    return m_openContextMenuRequested;
}

muse::async::Channel<au::trackedit::TrackId> TrackNavigationController::openRulerContextMenuRequested() const
{
    return m_openRulerContextMenuRequested;
}

TrackItemKey TrackNavigationController::focusedItemKey() const
{
    return m_focus.itemKey().value_or(TrackItemKey { m_focus.trackId, INVALID_TRACK_ITEM });
}

bool TrackNavigationController::isFocusedItemValid() const
{
    return m_focus.isItem();
}

bool TrackNavigationController::isFocusedItemLabel() const
{
    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return false;
    }

    return prj->track(m_focus.trackId)->type == TrackType::Label;
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

void TrackNavigationController::resetNavigation()
{
    MYLOG() << "====";

    m_savedItemStartTime = std::nullopt;
    navigationController()->setIsHighlight(false);
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
    MYLOG() << "====";

    if (navigateToAdjacentItem(true /*next*/)) {
        return;
    }

    MYLOG() << "no next item on the track, go to the next panel";

    commandDispatcher()->dispatch(muse::ui::NEXT_PANEL_COMMAND);
}

void TrackNavigationController::navigateToPrevPanel()
{
    MYLOG() << "====";

    if (navigateToAdjacentItem(false /*next*/)) {
        return;
    }

    MYLOG() << "no prev item on the track, go to the prev panel";

    commandDispatcher()->dispatch(muse::ui::PREV_PANEL_COMMAND);
}

bool TrackNavigationController::navigateToAdjacentItem(bool next)
{
    const std::optional<TrackItemKey> focusedItem = m_focus.itemKey();
    if (!focusedItem) {
        MYLOG() << "no focused item, the track items are not navigated";
        return false;
    }

    const TrackItemKeyList itemsKeys = sortedItemsKeys(focusedItem->trackId);

    for (size_t i = 0; i < itemsKeys.size(); ++i) {
        if (itemsKeys.at(i).itemId != focusedItem->itemId) {
            continue;
        }

        if (next) {
            if (i + 1 >= itemsKeys.size()) {
                return false;
            }
            setFocus(TrackFocus::item(itemsKeys.at(i + 1)), true /*highlight*/);
        } else {
            if (i == 0) {
                return false;
            }
            setFocus(TrackFocus::item(itemsKeys.at(i - 1)), true /*highlight*/);
        }

        m_savedItemStartTime = std::nullopt;
        return true;
    }

    return false;
}

void TrackNavigationController::navigateToPrevTrack()
{
    MYLOG() << "====";

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
                setFocus(TrackFocus::track(trackList.back().id), true /*highlight*/);
            } else {
                setFocus(TrackFocus::track(trackList.at(i - 1).id), true /*highlight*/);
            }
            return;
        }
    }
}

void TrackNavigationController::navigateToNextTrack()
{
    MYLOG() << "====";

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
                setFocus(TrackFocus::track(trackList[i].id), true /*highlight*/);
            } else {
                setFocus(TrackFocus::track(trackList.front().id), true /*highlight*/);
            }
            return;
        }
    }
}

void TrackNavigationController::navigateToFirstTrack()
{
    MYLOG() << "====";

    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    std::vector<Track> trackList = prj->trackList();
    if (!trackList.empty()) {
        setFocus(TrackFocus::track(trackList.front().id), true /*highlight*/);
    }
}

void TrackNavigationController::navigateToLastTrack()
{
    MYLOG() << "====";

    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    std::vector<Track> trackList = prj->trackList();
    if (!trackList.empty()) {
        setFocus(TrackFocus::track(trackList.back().id), true /*highlight*/);
    }
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
    MYLOG() << "====";

    if (m_focus.isRuler()) {
        navigateToAdjacentRuler(SelectionDirection::Up);
        return;
    }

    if (m_focus.isTrack()) {
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
        m_savedItemStartTime = itemStartTime(focusedItemKey());
    }

    const TrackId currentTrackId = m_focus.trackId;

    for (size_t i = 0; i < trackList.size(); ++i) {
        if (trackList[i].id == currentTrackId) {
            for (size_t j = i; j > 0; --j) {
                TrackId candidateId = trackList[j - 1].id;
                if (!isTrackItemsEmpty(candidateId)) {
                    TrackItemKey closest = findClosestItemOnTrack(candidateId, *m_savedItemStartTime);
                    setFocus(TrackFocus::item(closest), true /*highlight*/);
                    return;
                }
            }
            return;
        }
    }
}

void TrackNavigationController::navigateToBelowItem()
{
    MYLOG() << "====";

    if (m_focus.isRuler()) {
        navigateToAdjacentRuler(SelectionDirection::Down);
        return;
    }

    if (m_focus.isTrack()) {
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
        m_savedItemStartTime = itemStartTime(focusedItemKey());
    }

    const TrackId currentTrackId = m_focus.trackId;

    for (size_t i = 0; i < trackList.size(); ++i) {
        if (trackList[i].id == currentTrackId) {
            for (size_t j = i + 1; j < trackList.size(); ++j) {
                TrackId candidateId = trackList[j].id;
                if (!isTrackItemsEmpty(candidateId)) {
                    TrackItemKey closest = findClosestItemOnTrack(candidateId, *m_savedItemStartTime);
                    setFocus(TrackFocus::item(closest), true /*highlight*/);
                    return;
                }
            }
            return;
        }
    }
}

void TrackNavigationController::navigateToAdjacentRuler(SelectionDirection direction)
{
    MYLOG() << "====";

    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    const std::vector<Track> trackList = prj->trackList();
    const auto current = std::ranges::find(trackList, m_focus.trackId, &Track::id);
    if (current == trackList.end()) {
        return;
    }

    //! NOTE: label tracks have no vertical ruler, so they are skipped, as the tracks without items are for the items
    const int count = static_cast<int>(trackList.size());
    const int step = direction == SelectionDirection::Up ? -1 : 1;
    for (int pos = static_cast<int>(std::distance(trackList.begin(), current)) + step; pos >= 0 && pos < count; pos += step) {
        if (trackList[pos].type != TrackType::Label) {
            setFocus(TrackFocus::ruler(trackList[pos].id), true /*highlight*/);
            return;
        }
    }
}

void TrackNavigationController::navigateToFirstItem()
{
    MYLOG() << "====";

    TrackItemKeyList itemsKeys = sortedItemsKeys(m_focus.trackId);

    if (itemsKeys.empty()) {
        navigateToNextTrack();
        return;
    }

    setFocus(TrackFocus::item(itemsKeys.front()), true /*highlight*/);
}

void TrackNavigationController::navigateToLastItem()
{
    MYLOG() << "====";

    TrackItemKeyList itemsKeys = sortedItemsKeys(m_focus.trackId);

    if (itemsKeys.empty()) {
        navigateToPrevTrack();
        return;
    }

    setFocus(TrackFocus::item(itemsKeys.back()), true /*highlight*/);
}

void TrackNavigationController::replaceSelection()
{
    const TrackItemKey focusedKey = focusedItemKey();
    const bool isTrackPanel = !m_focus.isItem();

    bool isSelect = false;

    if (!isTrackPanel) {
        if (isFocusedItemLabel()) {
            LabelKeyList selectedLabels = selectionController()->selectedLabels();
            isSelect = !muse::contains(selectedLabels, focusedKey);
            selectionController()->setSelectedLabels(isSelect ? LabelKeyList { focusedKey } : LabelKeyList {});

            //! reset clips
            selectionController()->setSelectedClips({ });
        } else {
            ClipKeyList selectedClips = selectionController()->selectedClips();
            isSelect = !muse::contains(selectedClips, focusedKey);
            selectionController()->setSelectedClips(isSelect ? ClipKeyList { focusedKey } : ClipKeyList {});

            //! reset labels
            selectionController()->setSelectedLabels({});
        }
    } else {
        TrackIdList selectedTracks = selectionController()->selectedTracks();
        isSelect = !muse::contains(selectedTracks, focusedKey.trackId);
        selectionController()->setSelectedTracks(isSelect ? TrackIdList { focusedKey.trackId } : TrackIdList {});
    }

    m_lastSelectedTrack = isSelect ? std::optional<TrackId>(focusedKey.trackId) : std::nullopt;
    m_lastSelectedItem = (isSelect && !isTrackPanel) ? focusedKey : TrackItemKey {};
}

void TrackNavigationController::toggleSelection()
{
    const TrackItemKey focusedKey = focusedItemKey();
    const bool isTrackPanel = !m_focus.isItem();

    if (!isTrackPanel) {
        if (isFocusedItemLabel()) {
            LabelKeyList selectedLabels = selectionController()->selectedLabels();
            if (muse::contains(selectedLabels, focusedKey)) {
                selectionController()->removeLabelSelection(focusedKey);
            } else {
                selectionController()->addSelectedLabel(focusedKey);
            }
        } else {
            ClipKeyList selectedClips = selectionController()->selectedClips();
            if (muse::contains(selectedClips, focusedKey)) {
                selectionController()->removeClipSelection(focusedKey);
            } else {
                selectionController()->addSelectedClip(focusedKey);
            }
        }

        m_lastSelectedItem = focusedKey;
    } else {
        TrackIdList selectedTracks = selectionController()->selectedTracks();
        const TrackId focusedTrack = focusedKey.trackId;
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
    const TrackItemKey focusedKey = focusedItemKey();
    const bool isTrackPanel = !m_focus.isItem();

    bool isSelect = false;

    if (!isTrackPanel) {
        TrackItemKeyList range = itemKeysInRange(m_lastSelectedItem, focusedKey);
        if (range.empty()) {
            m_lastSelectedItem = focusedKey;
            range.push_back(focusedKey);
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
        TrackId focusedTrack = focusedKey.trackId;

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

    m_lastSelectedTrack = isSelect ? std::optional<TrackId>(focusedKey.trackId) : std::nullopt;
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
    MYLOG() << "track: " << m_focus.trackId << ", item: " << focusedItemKey().itemId;

    if (m_focus.trackId == INVALID_TRACK) {
        return;
    }

    m_openContextMenuRequested.send(focusedItemKey());
}

void TrackNavigationController::openContextMenuForFocusedRuler()
{
    MYLOG() << "track: " << m_focus.trackId;

    if (m_focus.trackId == INVALID_TRACK) {
        return;
    }

    m_openRulerContextMenuRequested.send(m_focus.trackId);
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
    setFocus(TrackFocus::track(trackId));
}
