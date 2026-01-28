/*
* Audacity: A Digital Audio Editor
*/
#include "trackclipslistmodel.h"

#include "global/realfn.h"
#include "global/async/async.h"

#include "types/projectscenetypes.h"

#include "log.h"

using namespace au::projectscene;
using namespace au::trackedit;

constexpr double MIN_CLIP_WIDTH = 3.0;

static const muse::Uri EDIT_PITCH_AND_SPEED_URI("audacity://projectscene/editpitchandspeed");

TrackClipsListModel::TrackClipsListModel(QObject* parent)
    : TrackItemsListModel(parent)
{
}

void TrackClipsListModel::onInit()
{
    selectionController()->clipsSelected().onReceive(this, [this](const ClipKeyList& keyList) {
        if (keyList.empty()) {
            resetSelectedClips();
        }

        onSelectedItems(keyList);
    });

    projectSceneConfiguration()->clipStyleChanged().onReceive(this, [this](const ClipStyles::Style& style) {
        Q_UNUSED(style);
        emit clipStyleChanged();
        update();
    });

    projectSceneConfiguration()->stereoHeightsPrefChanged().onNotify(this, [this] {
        emit asymmetricStereoHeightsPossibleChanged();
    });

    projectSceneConfiguration()->asymmetricStereoHeightsWorkspacesChanged().onNotify(this, [this] {
        emit asymmetricStereoHeightsPossibleChanged();
    });

    workspacesManager()->currentWorkspaceChanged().onNotify(this, [this]() {
        emit asymmetricStereoHeightsPossibleChanged();
    });

    dispatcher()->reg(this, "rename-clip", [this]() {
        requestItemTitleChange();
    });

    uiConfiguration()->currentThemeChanged().onNotify(this, [this]() {
        emit isContrastFocusBorderEnabledChanged();
    });
}

void TrackClipsListModel::onReload()
{
    ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    IF_ASSERT_FAILED(prj) {
        return;
    }

    m_allClipList = prj->clipList(m_trackId);

    //! NOTE Clips in the track may not be in order (relative to startTime), here we arrange them.
    //! Accordingly, the indexes will change, we need to keep this in mind.
    //! To identify clips, clips have a key.
    std::sort(m_allClipList.begin(), m_allClipList.end(), [](const Clip& c1, const Clip& c2) {
        return c1.startTime < c2.startTime;
    });

    //! NOTE Reload everything if the list has changed completely
    m_allClipList.onChanged(this, [this]() {
        muse::async::Async::call(this, [this]() {
            reload();
        });
    }, muse::async::Asyncable::Mode::SetReplace);

    m_allClipList.onItemChanged(this, [this](const Clip& clip) {
        for (size_t i = 0; i < m_allClipList.size(); ++i) {
            if (m_allClipList.at(i).key != clip.key) {
                continue;
            }
            m_allClipList[i] = clip;
            break;
        }

        // LOGDA() << "clip: " << clip.key << ", startTime: " << clip.startTime;
        TrackClipItem* item = clipItemByKey(clip.key);
        if (item) {
            item->setClip(clip);
        }

        m_context->updateSelectedItemTime();

        updateItemsMetrics();
    }, muse::async::Asyncable::Mode::SetReplace);

    m_allClipList.onItemAdded(this, [this](const Clip& clip) {
        ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        muse::async::NotifyList<au::trackedit::Clip> newList = prj->clipList(m_trackId);
        for (size_t i = 0; i < newList.size(); ++i) {
            if (newList.at(i).key != clip.key) {
                continue;
            }

            m_allClipList.insert(m_allClipList.begin() + i, clip);

            update();

            break;
        }
    }, muse::async::Asyncable::Mode::SetReplace);

    m_allClipList.onItemRemoved(this, [this](const Clip& clip) {
        for (auto it = m_allClipList.begin(); it != m_allClipList.end(); ++it) {
            if (it->key == clip.key) {
                m_allClipList.erase(it);
                update();
                break;
            }
        }
    }, muse::async::Asyncable::Mode::SetReplace);

    update();
}

TrackClipItem* TrackClipsListModel::clipItemByKey(const trackedit::ClipKey& k) const
{
    return static_cast<TrackClipItem*>(itemByKey(k));
}

void TrackClipsListModel::update()
{
    std::unordered_map<ClipId, TrackClipItem*> oldItems;
    for (int row = 0; row < m_items.size(); ++row) {
        TrackClipItem* clipItem = static_cast<TrackClipItem*>(m_items[row]);
        oldItems.emplace(clipItem->key().key.itemId, clipItem);
    }

    QList<TrackClipItem*> newList;
    bool isStereo = false;

    // Building a new list, reusing exiting clips
    for (const au::trackedit::Clip& c : m_allClipList) {
        auto it = oldItems.find(c.key.itemId);
        TrackClipItem* item = nullptr;

        if (it != oldItems.end()) {
            item = it->second;
            oldItems.erase(it);
        } else {
            item = new TrackClipItem(this);
        }

        item->setClip(c);
        newList.append(item);
        isStereo |= c.stereo;
    }

    // Removing deleted or moved clips
    // Item deletion should be postponed, so QML is updated correctly
    QList<TrackClipItem*> cleanupList;
    for (auto& [id, item] : oldItems) {
        int row = m_items.indexOf(item);
        if (row >= 0) {
            beginRemoveRows(QModelIndex(), row, row);
            m_items.removeAt(row);
            endRemoveRows();
        }
        cleanupList.append(item);
    }

    // Sorting clips with a notification for each moved clip
    for (int i = 0; i < newList.size(); ++i) {
        TrackClipItem* item = newList[i];
        if (i < m_items.size() && m_items[i] == item) {
            // TODO: is it possible to know if update is neccessary?
            QModelIndex idx = index(i);
            emit dataChanged(idx, idx);
        } else {
            // If the clip was already present, then moving
            int oldIndex = m_items.indexOf(item);
            if (oldIndex >= 0) {
                beginMoveRows(QModelIndex(), oldIndex, oldIndex, QModelIndex(), i > oldIndex ? i + 1 : i);
                m_items.move(oldIndex, i);
                endMoveRows();
            } else {
                beginInsertRows(QModelIndex(), i, i);
                m_items.insert(i, item);
                endInsertRows();
            }
        }
    }

    updateItemsMetrics();

    //! NOTE We need to update the selected items
    //! to take pointers to the items from the new list
    m_selectedItems.clear();
    onSelectedItems(selectionController()->selectedClips());

    if (m_isStereo != isStereo) {
        m_isStereo = isStereo;
        emit isStereoChanged();
    }

    muse::async::Async::call(this, [cleanupList]() {
        qDeleteAll(cleanupList);
    });
}

void TrackClipsListModel::updateItemMetrics(ViewTrackItem* viewItem)
{
    TrackClipItem* item = static_cast<TrackClipItem*>(viewItem);

    ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    trackedit::Clip clip = prj->clip(item->key().key);
    if (!clip.isValid()) {
        return;
    }

    //! NOTE The first step is to calculate the position and width
    const double cacheTime = cacheBufferPx() / m_context->zoom();

    ClipTime time;
    time.startTime = clip.startTime;
    time.endTime = clip.endTime;
    time.itemStartTime = std::max(clip.startTime, (m_context->frameStartTime() - cacheTime));
    time.itemEndTime = std::min(clip.endTime, (m_context->frameEndTime() + cacheTime));

    if (selectionController()->isDataSelectedOnTrack(m_trackId)) {
        time.selectionStartTime = selectionController()->dataSelectedStartTime();
        time.selectionEndTime = selectionController()->dataSelectedEndTime();
    }
    item->setIntersectsSelection(muse::contains(selectionController()->clipsIntersectingRangeSelection(), item->key().key));

    item->setTime(time);
    item->setX(m_context->timeToPosition(time.itemStartTime));
    item->setWidth((time.itemEndTime - time.itemStartTime) * m_context->zoom());
    item->setLeftVisibleMargin(std::max(m_context->frameStartTime() - time.itemStartTime, 0.0) * m_context->zoom());
    item->setRightVisibleMargin(std::max(time.itemEndTime - m_context->frameEndTime(), 0.0) * m_context->zoom());
}

bool TrackClipsListModel::changeClipTitle(const ClipKey& key, const QString& newTitle)
{
    return trackeditInteraction()->changeClipTitle(key.key, newTitle);
}

bool TrackClipsListModel::isKeyboardTriggered() const
{
    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    if (!prj) {
        return 0;
    }

    auto vs = prj->viewState();

    return muse::RealIsEqual(vs->itemEditStartTimeOffset(), -1.0);
}

void TrackClipsListModel::openClipPitchEdit(const ClipKey& key)
{
    selectClip(key);

    if (interactive()->isOpened(EDIT_PITCH_AND_SPEED_URI).val) {
        return;
    }

    muse::UriQuery query(EDIT_PITCH_AND_SPEED_URI);
    query.addParam("trackId", muse::Val(std::to_string(key.key.trackId)));
    query.addParam("clipId", muse::Val(std::to_string(key.key.itemId)));
    query.addParam("focusItemName", muse::Val("pitch"));

    interactive()->open(query);
}

void TrackClipsListModel::resetClipPitch(const ClipKey& key)
{
    trackeditInteraction()->resetClipPitch(key.key);
}

void TrackClipsListModel::openClipSpeedEdit(const ClipKey& key)
{
    selectClip(key);

    if (interactive()->isOpened(EDIT_PITCH_AND_SPEED_URI).val) {
        return;
    }

    muse::UriQuery query(EDIT_PITCH_AND_SPEED_URI);
    query.addParam("trackId", muse::Val(std::to_string(key.key.trackId)));
    query.addParam("clipId", muse::Val(std::to_string(key.key.itemId)));
    query.addParam("focusItemName", muse::Val("speed"));

    interactive()->open(query);
}

void TrackClipsListModel::resetClipSpeed(const ClipKey& key)
{
    trackeditInteraction()->resetClipSpeed(key.key);
}

bool TrackClipsListModel::asymmetricStereoHeightsPossible() const
{
    auto pref = projectSceneConfiguration()->stereoHeightsPref();
    if (pref == projectscene::StereoHeightsPref::AsymmetricStereoHeights::ALWAYS) {
        return true;
    } else if (pref == projectscene::StereoHeightsPref::AsymmetricStereoHeights::WORKSPACE_DEPENDENT) {
        std::string currentWorkspace = workspacesManager()->currentWorkspace()->name();
        if (muse::contains(projectSceneConfiguration()->asymmetricStereoHeightsWorkspaces(), currentWorkspace)) {
            return true;
        }
    }

    return false;
}

bool TrackClipsListModel::isContrastFocusBorderEnabled() const
{
    return !uiConfiguration()->isDarkMode();
}

au::projectscene::ClipKey TrackClipsListModel::updateClipTrack(ClipKey clipKey) const
{
    ITrackeditProjectPtr trackeditPrj = globalContext()->currentTrackeditProject();
    if (!trackeditPrj) {
        return clipKey;
    }

    auto allTracks = trackeditPrj->trackIdList();

    for (const auto& trackId : allTracks) {
        auto clips = trackeditPrj->clipList(trackId);

        for (const auto& clip : clips) {
            if (clip.key.itemId == clipKey.key.itemId) {
                ClipKey updatedKey;
                updatedKey.key.trackId = trackId;
                updatedKey.key.itemId = clip.key.itemId;
                return updatedKey;
            }
        }
    }

    return clipKey;
}

/*!
 * \brief Moves all selected clips
 * \param key - the key from which the offset will be calculated to move all clips

    Calculate offset of clip that's being grabbed
    and apply it to all selected clips
 */
bool TrackClipsListModel::moveSelectedClips(const ClipKey& key, bool completed)
{
    TrackClipItem* item = clipItemByKey(key.key);
    if (!item) {
        return false;
    }

    auto project = globalContext()->currentProject();
    IF_ASSERT_FAILED(project) {
        return false;
    }

    auto vs = project->viewState();
    IF_ASSERT_FAILED(vs) {
        return false;
    }

    bool clipsMovedToOtherTrack = false;
    // Clips can only be moved to audio tracks (Mono and Stereo)
    TrackItemsListModel::MoveOffset moveOffset = calculateMoveOffset(item, key, {
        trackedit::TrackType::Mono,
        trackedit::TrackType::Stereo
    }, completed);

    if (vs->moveInitiated()) {
        if (selectionController()->timeSelectionIsNotEmpty()) {
            trackeditInteraction()->moveRangeSelection(moveOffset.timeOffset, completed);
        } else {
            ClipKeyList selectedClips = selectionController()->selectedClipsInTrackOrder();
            muse::RetVal<ClipKeyList> result = trackeditInteraction()->moveClips(selectedClips, moveOffset.timeOffset,
                                                                                 moveOffset.trackOffset,
                                                                                 completed, clipsMovedToOtherTrack);
            if (result.ret) {
                selectionController()->setSelectedClips(result.val);
            }
        }
    }

    // Update key if clip moved to another track
    ClipKey updatedKey = key;
    if (clipsMovedToOtherTrack && !completed) {
        updatedKey = updateClipTrack(key);

        // Reconnect with updated key
        if (m_autoScrollConnection) {
            disconnectAutoScroll();
        }
        m_autoScrollConnection = connect(m_context, &TimelineContext::frameTimeChanged, [this, updatedKey](){
            moveSelectedClips(updatedKey, false);
        });
    } else if ((completed && m_autoScrollConnection)) {
        disconnectAutoScroll();
    } else if (!m_autoScrollConnection && !completed) {
        m_autoScrollConnection = connect(m_context, &TimelineContext::frameTimeChanged, [this, key](){ moveSelectedClips(key, false); });
    }

    return clipsMovedToOtherTrack;
}

bool TrackClipsListModel::trimLeftClip(const ClipKey& key, bool completed, ClipBoundary::Action action)
{
    TrackClipItem* item = clipItemByKey(key.key);
    IF_ASSERT_FAILED(item) {
        return false;
    }

    auto project = globalContext()->currentProject();
    IF_ASSERT_FAILED(project) {
        return false;
    }

    auto vs = project->viewState();
    IF_ASSERT_FAILED(vs) {
        return false;
    }

    UndoPushType undoType = UndoPushType::NONE;
    double newStartTime;
    if (isKeyboardTriggered()) {
        secs_t offset = 1 / m_context->zoom();

        if (action == ClipBoundary::Action::Expand) {
            offset = -offset;
        }

        newStartTime = item->time().startTime + offset;

        Direction direction = Direction::Left;
        if (action == ClipBoundary::Action::Shrink) {
            direction = Direction::Right;
        }
        bool snapEnabled = vs->isSnapEnabled();
        if (snapEnabled) {
            newStartTime = m_context->singleStepToTime(m_context->timeToPosition(newStartTime), direction, vs->snap().val);
        }

        if (vs->lastEditedClip().isValid() && vs->lastEditedClip() == key.key) {
            undoType = UndoPushType::CONSOLIDATE;
        }
    } else {
        newStartTime = m_context->mousePositionTime() - vs->itemEditStartTimeOffset();
        if (vs->isSnapEnabled()) {
            newStartTime = m_context->applySnapToTime(newStartTime);
        } else {
            newStartTime = m_context->applySnapToItem(newStartTime);
        }
    }

    double minClipTime = MIN_CLIP_WIDTH / m_context->zoom();
    double newClipTime = item->time().endTime - newStartTime;
    if (newClipTime < minClipTime) {
        newStartTime = item->time().endTime - minClipTime;
    }

    newStartTime = std::max(newStartTime, 0.0);

    bool ok = trackeditInteraction()->trimClipLeft(key.key, newStartTime - item->time().startTime, minClipTime, completed, undoType);

    if (ok) {
        vs->setLastEditedClip(key.key);
    }

    handleAutoScroll(ok, completed, [this, key]() {
        trimLeftClip(key, false);
    });

    return ok;
}

bool TrackClipsListModel::trimRightClip(const ClipKey& key, bool completed, ClipBoundary::Action action)
{
    TrackClipItem* item = clipItemByKey(key.key);
    IF_ASSERT_FAILED(item) {
        return false;
    }

    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return false;
    }

    UndoPushType undoType = UndoPushType::NONE;
    double newEndTime;
    if (isKeyboardTriggered()) {
        secs_t offset = 1 / m_context->zoom();

        if (action == ClipBoundary::Action::Expand) {
            offset = -offset;
        }

        newEndTime = item->time().endTime - offset;

        Direction direction = Direction::Left;
        if (action == ClipBoundary::Action::Expand) {
            direction = Direction::Right;
        }
        bool snapEnabled = vs->isSnapEnabled();
        if (snapEnabled) {
            newEndTime = m_context->singleStepToTime(m_context->timeToPosition(newEndTime), direction, vs->snap().val);
        }

        if (vs->lastEditedClip().isValid() && vs->lastEditedClip() == key.key) {
            undoType = UndoPushType::CONSOLIDATE;
        }
    } else {
        newEndTime = m_context->mousePositionTime() + vs->itemEditEndTimeOffset();
        if (vs->isSnapEnabled()) {
            newEndTime = m_context->applySnapToTime(newEndTime);
        } else {
            newEndTime = m_context->applySnapToItem(newEndTime);
        }
    }

    double minClipTime = MIN_CLIP_WIDTH / m_context->zoom();
    double newClipTime = newEndTime - item->time().startTime;
    if (newClipTime < minClipTime) {
        newEndTime = item->time().startTime + minClipTime;
    }

    bool ok = trackeditInteraction()->trimClipRight(key.key, item->time().endTime - newEndTime, minClipTime, completed, undoType);

    if (ok) {
        vs->setLastEditedClip(key.key);
    }

    handleAutoScroll(ok, completed, [this, key]() {
        trimRightClip(key, false);
    });

    return ok;
}

bool TrackClipsListModel::stretchLeftClip(const ClipKey& key, bool completed, ClipBoundary::Action action)
{
    TrackClipItem* item = clipItemByKey(key.key);
    IF_ASSERT_FAILED(item) {
        return false;
    }

    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return false;
    }

    UndoPushType undoType = UndoPushType::NONE;
    double newStartTime;
    if (isKeyboardTriggered()) {
        secs_t offset = 1 / m_context->zoom();

        if (action == ClipBoundary::Action::Expand) {
            offset = -offset;
        }

        newStartTime = item->time().startTime + offset;

        Direction direction = Direction::Left;
        if (action == ClipBoundary::Action::Shrink) {
            direction = Direction::Right;
        }
        bool snapEnabled = vs->isSnapEnabled();
        if (snapEnabled) {
            newStartTime = m_context->singleStepToTime(m_context->timeToPosition(newStartTime), direction, vs->snap().val);
        }

        if (vs->lastEditedClip().isValid() && vs->lastEditedClip() == key.key) {
            undoType = UndoPushType::CONSOLIDATE;
        }
    } else {
        newStartTime = m_context->mousePositionTime() - vs->itemEditStartTimeOffset();
        if (vs->isSnapEnabled()) {
            newStartTime = m_context->applySnapToTime(newStartTime);
        } else {
            newStartTime = m_context->applySnapToItem(newStartTime);
        }
    }

    double minClipTime = MIN_CLIP_WIDTH / m_context->zoom();
    double newClipTime = item->time().endTime - newStartTime;
    if (newClipTime < minClipTime) {
        newStartTime = item->time().endTime - minClipTime;
    }

    newStartTime = std::max(newStartTime, 0.0);

    bool ok = trackeditInteraction()->stretchClipLeft(key.key, newStartTime - item->time().startTime, minClipTime, completed, undoType);

    if (ok) {
        vs->setLastEditedClip(key.key);
    }

    handleAutoScroll(ok, completed, [this, key]() {
        stretchLeftClip(key, false);
    });

    return ok;
}

bool TrackClipsListModel::stretchRightClip(const ClipKey& key, bool completed, ClipBoundary::Action action)
{
    TrackClipItem* item = clipItemByKey(key.key);
    IF_ASSERT_FAILED(item) {
        return false;
    }

    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return false;
    }

    UndoPushType undoType = UndoPushType::NONE;
    double newEndTime;
    if (isKeyboardTriggered()) {
        secs_t offset = 1 / m_context->zoom();

        if (action == ClipBoundary::Action::Expand) {
            offset = -offset;
        }

        newEndTime = item->time().endTime - offset;

        Direction direction = Direction::Left;
        if (action == ClipBoundary::Action::Expand) {
            direction = Direction::Right;
        }
        bool snapEnabled = vs->isSnapEnabled();
        if (snapEnabled) {
            newEndTime = m_context->singleStepToTime(m_context->timeToPosition(newEndTime), direction, vs->snap().val);
        }

        if (vs->lastEditedClip().isValid() && vs->lastEditedClip() == key.key) {
            undoType = UndoPushType::CONSOLIDATE;
        }
    } else {
        newEndTime = m_context->mousePositionTime() + vs->itemEditEndTimeOffset();
        if (vs->isSnapEnabled()) {
            newEndTime = m_context->applySnapToTime(newEndTime);
        } else {
            newEndTime = m_context->applySnapToItem(newEndTime);
        }
    }

    double minClipTime = MIN_CLIP_WIDTH / m_context->zoom();
    double newClipTime = newEndTime - item->time().startTime;
    if (newClipTime < minClipTime) {
        newEndTime = item->time().startTime + minClipTime;
    }

    bool ok = trackeditInteraction()->stretchClipRight(key.key, item->time().endTime - newEndTime, minClipTime, completed, undoType);

    if (ok) {
        vs->setLastEditedClip(key.key);
    }

    handleAutoScroll(ok, completed, [this, key]() {
        stretchRightClip(key, false);
    });

    return ok;
}

void TrackClipsListModel::selectClip(const ClipKey& key)
{
    Qt::KeyboardModifiers modifiers = keyboardModifiers();

    constexpr auto complete = true;
    const auto clipGroupId = trackeditInteraction()->clipGroupId(key.key);
    if (clipGroupId != -1) {
        //! NOTE: clip belongs to a group, select the whole group
        if (modifiers.testFlag(Qt::ShiftModifier)) {
            for (const auto& groupClipKey : trackeditInteraction()->clipsInGroup(clipGroupId)) {
                selectionController()->addSelectedClip(groupClipKey);
            }
        } else {
            selectionController()->resetSelectedLabels();
            selectionController()->setSelectedClips(trackeditInteraction()->clipsInGroup(clipGroupId), complete);
            selectionController()->setFocusedTrack(key.key.trackId);
        }
    } else {
        if (modifiers.testFlag(Qt::ShiftModifier)) {
            selectionController()->addSelectedClip(key.key);
        } else {
            if (muse::contains(selectionController()->selectedClips(), key.key)) {
                return;
            }
            selectionController()->resetSelectedLabels();
            selectionController()->setSelectedClips(ClipKeyList({ key.key }), complete);
        }
    }
}

void TrackClipsListModel::resetSelectedClips()
{
    clearSelectedItems();
    selectionController()->resetSelectedClips();
}

TrackItemKeyList TrackClipsListModel::getSelectedItemKeys() const
{
    return selectionController()->selectedClips();
}

bool TrackClipsListModel::isStereo() const
{
    return m_isStereo;
}

ClipStyles::Style TrackClipsListModel::clipStyle() const
{
    return projectSceneConfiguration()->clipStyle();
}
