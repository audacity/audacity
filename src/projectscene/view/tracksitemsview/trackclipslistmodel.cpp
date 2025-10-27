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

constexpr int CACHE_BUFFER_PX = 200;
constexpr double MIN_CLIP_WIDTH = 3.0;
constexpr double MOVE_THRESHOLD = 3.0;

static const muse::Uri EDIT_PITCH_AND_SPEED_URI("audacity://projectscene/editpitchandspeed");

TrackClipsListModel::TrackClipsListModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

TrackClipsListModel::~TrackClipsListModel()
{
    disconnectAutoScroll();
}

void TrackClipsListModel::init()
{
    IF_ASSERT_FAILED(m_trackId >= 0) {
        return;
    }

    onSelectedClips(selectionController()->selectedClips());
    selectionController()->clipsSelected().onReceive(this, [this](const ClipKeyList& keyList) {
        if (keyList.empty()) {
            resetSelectedClips();
        }

        onSelectedClips(keyList);
    });

    selectionController()->dataSelectedStartTimeChanged().onReceive(this, [this](trackedit::secs_t time) {
        Q_UNUSED(time);
        updateItemsMetrics();
    });
    selectionController()->dataSelectedEndTimeChanged().onReceive(this, [this](trackedit::secs_t time) {
        Q_UNUSED(time);
        updateItemsMetrics();
    });
    selectionController()->tracksSelected().onReceive(this, [this](const TrackIdList&) {
        updateItemsMetrics();
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

    dispatcher()->reg(this, "rename-clip", this, &TrackClipsListModel::requestClipTitleChange);

    uiConfiguration()->currentThemeChanged().onNotify(this, [this]() {
        emit isContrastFocusBorderEnabledChanged();
    });

    reload();
}

void TrackClipsListModel::reload()
{
    if (m_trackId < 0) {
        return;
    }

    disconnectAutoScroll();

    ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    prj->trackChanged().onReceive(this, [this](const au::trackedit::Track& track) {
        if (track.id == m_trackId) {
            reload();
        }
    }, muse::async::Asyncable::Mode::SetReplace);

    prj->trackRemoved().onReceive(this, [this](const au::trackedit::Track& track) {
        if (track.id == m_trackId) {
            m_trackId = -1;
        }
    }, muse::async::Asyncable::Mode::SetReplace);

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
        TrackClipItem* item = itemByKey(clip.key);
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

TrackClipItem* TrackClipsListModel::itemByKey(const trackedit::ClipKey& k) const
{
    for (TrackClipItem* item : std::as_const(m_clipList)) {
        if (item->clip().key != k) {
            continue;
        }
        return item;
    }
    return nullptr;
}

int TrackClipsListModel::indexByKey(const trackedit::ClipKey& k) const
{
    for (int i = 0; i < m_clipList.size(); ++i) {
        if (m_clipList.at(i)->clip().key == k) {
            return i;
        }
    }
    return -1;
}

Qt::KeyboardModifiers TrackClipsListModel::keyboardModifiers() const
{
    Qt::KeyboardModifiers modifiers = QApplication::keyboardModifiers();

    //! NOTE: always treat simultaneously pressed Ctrl and Shift as Ctrl
    if (modifiers.testFlag(Qt::ShiftModifier) && modifiers.testFlag(Qt::ControlModifier)) {
        modifiers = Qt::ControlModifier;
    }

    return modifiers;
}

void TrackClipsListModel::update()
{
    std::unordered_map<ClipId, TrackClipItem*> oldItems;
    for (int row = 0; row < m_clipList.size(); ++row) {
        oldItems.emplace(m_clipList[row]->key().key.objectId, m_clipList[row]);
    }

    QList<TrackClipItem*> newList;
    bool isStereo = false;

    // Building a new list, reusing exiting clips
    for (const au::trackedit::Clip& c : m_allClipList) {
        auto it = oldItems.find(c.key.objectId);
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
        int row = m_clipList.indexOf(item);
        if (row >= 0) {
            beginRemoveRows(QModelIndex(), row, row);
            m_clipList.removeAt(row);
            endRemoveRows();
        }
        cleanupList.append(item);
    }

    // Sorting clips with a notification for each moved clip
    for (int i = 0; i < newList.size(); ++i) {
        TrackClipItem* item = newList[i];
        if (i < m_clipList.size() && m_clipList[i] == item) {
            // TODO: is it possible to know if update is neccessary?
            QModelIndex idx = index(i);
            emit dataChanged(idx, idx);
        } else {
            // If the clip was already present, then moving
            int oldIndex = m_clipList.indexOf(item);
            if (oldIndex >= 0) {
                beginMoveRows(QModelIndex(), oldIndex, oldIndex, QModelIndex(), i > oldIndex ? i + 1 : i);
                m_clipList.move(oldIndex, i);
                endMoveRows();
            } else {
                beginInsertRows(QModelIndex(), i, i);
                m_clipList.insert(i, item);
                endInsertRows();
            }
        }
    }

    updateItemsMetrics();

    //! NOTE We need to update the selected items
    //! to take pointers to the items from the new list
    m_selectedItems.clear();
    onSelectedClips(selectionController()->selectedClips());

    if (m_isStereo != isStereo) {
        m_isStereo = isStereo;
        emit isStereoChanged();
    }

    muse::async::Async::call(this, [cleanupList]() {
        qDeleteAll(cleanupList);
    });
}

void TrackClipsListModel::updateItemsMetrics()
{
    for (int i = 0; i < m_clipList.size(); ++i) {
        TrackClipItem* item = m_clipList[i];
        updateItemsMetrics(item);
    }
}

void TrackClipsListModel::updateItemsMetrics(TrackClipItem* item)
{
    //! NOTE The first step is to calculate the position and width
    const double cacheTime = CACHE_BUFFER_PX / m_context->zoom();

    const trackedit::Clip& clip = item->clip();

    ClipTime time;
    time.startTime = clip.startTime;
    time.endTime = clip.endTime;
    time.itemStartTime = std::max(clip.startTime, (m_context->frameStartTime() - cacheTime));
    time.itemEndTime = std::min(clip.endTime, (m_context->frameEndTime() + cacheTime));

    if (selectionController()->isDataSelectedOnTrack(m_trackId)) {
        time.selectionStartTime = selectionController()->dataSelectedStartTime();
        time.selectionEndTime = selectionController()->dataSelectedEndTime();
    }

    item->setTime(time);
    item->setX(m_context->timeToPosition(time.itemStartTime));
    item->setWidth((time.itemEndTime - time.itemStartTime) * m_context->zoom());
    item->setLeftVisibleMargin(std::max(m_context->frameStartTime() - time.itemStartTime, 0.0) * m_context->zoom());
    item->setRightVisibleMargin(std::max(time.itemEndTime - m_context->frameEndTime(), 0.0) * m_context->zoom());
}

void TrackClipsListModel::positionViewAtClip(const Clip& clip)
{
    double frameStartTime = m_context->frameStartTime();
    double frameEndTime = m_context->frameEndTime();

    if (frameStartTime <= clip.startTime && frameEndTime > clip.startTime) {
        return;
    }

    double OFFSET = (frameEndTime - frameStartTime) / 4.0;
    double newTime = std::max(clip.startTime - OFFSET, 0.0);

    m_context->moveToFrameTime(newTime);
}

int TrackClipsListModel::rowCount(const QModelIndex&) const
{
    return static_cast<int>(m_clipList.size());
}

QHash<int, QByteArray> TrackClipsListModel::roleNames() const
{
    static QHash<int, QByteArray> roles
    {
        { ClipItemRole, "item" }
    };
    return roles;
}

QVariant TrackClipsListModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid()) {
        return QVariant();
    }

    switch (role) {
    case ClipItemRole: {
        TrackClipItem* item = m_clipList.at(index.row());
        return QVariant::fromValue(item);
    }
    default:
        break;
    }

    return QVariant();
}

void TrackClipsListModel::onTimelineZoomChanged()
{
    updateItemsMetrics();
}

void TrackClipsListModel::onTimelineFrameTimeChanged()
{
    updateItemsMetrics();
}

void TrackClipsListModel::setSelectedItems(const QList<TrackClipItem*>& items)
{
    for (auto& selectedItem : m_selectedItems) {
        selectedItem->setSelected(false);
    }
    m_selectedItems = items;
    for (auto& selectedItem : m_selectedItems) {
        selectedItem->setSelected(true);
    }
}

void TrackClipsListModel::addSelectedItem(TrackClipItem* item)
{
    item->setSelected(true);
    m_selectedItems.append(item);
}

void TrackClipsListModel::clearSelectedItems()
{
    for (auto& selectedItem : m_selectedItems) {
        selectedItem->setSelected(false);
    }
    m_selectedItems.clear();
}

bool TrackClipsListModel::changeClipTitle(const ClipKey& key, const QString& newTitle)
{
    bool ok = trackeditInteraction()->changeClipTitle(key.key, newTitle);
    return ok;
}

QVariant TrackClipsListModel::next(const ClipKey& key) const
{
    return neighbor(key, 1);
}

QVariant TrackClipsListModel::prev(const ClipKey& key) const
{
    return neighbor(key, -1);
}

QVariant TrackClipsListModel::neighbor(const ClipKey& key, int offset) const
{
    auto it = std::find_if(m_clipList.begin(), m_clipList.end(), [key](TrackClipItem* clip) {
        return clip->key().key.objectId == key.key.objectId;
    });

    if (it == m_clipList.end()) {
        return QVariant();
    }

    int sortedIndex = std::distance(m_clipList.begin(), it) + offset;
    if (sortedIndex < 0 || sortedIndex >= m_clipList.size()) {
        return QVariant();
    }

    return QVariant::fromValue(m_clipList[sortedIndex]);
}

TrackClipsListModel::MoveOffset TrackClipsListModel::calculateMoveOffset(const TrackClipItem* item,
                                                               const ClipKey& key,
                                                               bool completed) const
{
    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    if (!prj) {
        return MoveOffset{};
    }

    auto vs = prj->viewState();

    MoveOffset moveOffset {
        calculateTimePositionOffset(item),
        completed ? 0 : calculateTrackPositionOffset(key)
    };

    secs_t positionOffsetX = moveOffset.timeOffset * m_context->zoom();
    if (!vs->moveInitiated() && (muse::RealIsEqualOrMore(std::abs(positionOffsetX), MOVE_THRESHOLD) || moveOffset.trackOffset != 0)) {
        vs->setMoveInitiated(true);
    } else if (!vs->moveInitiated()) {
        moveOffset.timeOffset = 0.0;
    }

    return moveOffset;
}

int TrackClipsListModel::calculateTrackPositionOffset(const ClipKey& key) const
{
    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    if (!prj) {
        return 0;
    }

    IProjectViewStatePtr vs = prj->viewState();
    double yPos = vs->mousePositionY();
    int trackVerticalPosition = vs->trackVerticalPosition(key.key.trackId);
    TrackIdList tracks = vs->tracksInRange(trackVerticalPosition + 2, yPos);

    if (!tracks.size()) {
        return 0;
    }

    bool pointingAtEmptySpace = yPos > vs->totalTrackHeight().val - vs->tracksVerticalOffset().val;
    const auto numTracks = static_cast<int>(tracks.size());
    int trackPositionOffset = pointingAtEmptySpace ? numTracks : numTracks - 1;

    if (!muse::RealIsEqualOrMore(yPos, trackVerticalPosition)) {
        trackPositionOffset = -trackPositionOffset;
    }

    return trackPositionOffset;
}

bool TrackClipsListModel::isKeyboardTriggered() const
{
    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    if (!prj) {
        return 0;
    }

    auto vs = prj->viewState();

    return muse::RealIsEqual(vs->objectEditStartTimeOffset(), -1.0);
}

void TrackClipsListModel::handleAutoScroll(bool ok,
                                      bool completed,
                                      const std::function<void()>& onAutoScrollFrame)
{
    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return;
    }

    // do not handle auto-scroll when using key-nav
    if (muse::RealIsEqual(vs->objectEditStartTimeOffset(), -1.0)) {
        return;
    }

    // handle auto-scroll over the edge
    if (!ok) {
        m_context->stopAutoScroll();
    } else {
        m_context->startAutoScroll(m_context->mousePositionTime());
    }

    if ((completed && m_autoScrollConnection) || !ok) {
        disconnectAutoScroll();
    } else if (!m_autoScrollConnection && !completed) {
        m_autoScrollConnection = connect(m_context, &TimelineContext::frameTimeChanged, onAutoScrollFrame);
    }
}

void TrackClipsListModel::disconnectAutoScroll()
{
    if (m_autoScrollConnection) {
        disconnect(m_autoScrollConnection);
        m_autoScrollConnection = QMetaObject::Connection();
    }
}

secs_t TrackClipsListModel::calculateTimePositionOffset(const TrackClipItem* item) const
{
    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return 0.0;
    }

    double newStartTime = m_context->mousePositionTime() - vs->objectEditStartTimeOffset();
    double duration = item->time().endTime - item->time().startTime;
    double newEndTime = newStartTime + duration;

    double snappedEndTime = newEndTime;
    double snappedStartTime = newStartTime;
    if (vs->isSnapEnabled()) {
        snappedStartTime = m_context->applySnapToTime(newStartTime);
    } else {
        snappedEndTime = m_context->applySnapToClip(newEndTime);
        snappedStartTime = m_context->applySnapToClip(newStartTime);
    }
    if (muse::RealIsEqual(snappedEndTime, newEndTime)) {
        newStartTime = snappedStartTime;
    } else if (muse::RealIsEqual(snappedStartTime, newStartTime)) {
        newStartTime = snappedEndTime - duration;
    } else {
        newStartTime
            = (!muse::RealIsEqualOrMore(std::abs(snappedStartTime - newStartTime), std::abs(snappedEndTime - newEndTime))
               ? snappedStartTime : snappedEndTime - duration);
    }

    secs_t timePositionOffset = newStartTime - item->time().startTime;

    constexpr auto limit = 1. / 192000.; // 1 sample at 192 kHz
    if (!muse::RealIsEqualOrMore(std::abs(timePositionOffset), limit)) {
        timePositionOffset = 0.0;
    }

    return timePositionOffset;
}

void TrackClipsListModel::openClipPitchEdit(const ClipKey& key)
{
    selectClip(key);

    if (interactive()->isOpened(EDIT_PITCH_AND_SPEED_URI).val) {
        return;
    }

    muse::UriQuery query(EDIT_PITCH_AND_SPEED_URI);
    query.addParam("trackId", muse::Val(std::to_string(key.key.trackId)));
    query.addParam("clipId", muse::Val(std::to_string(key.key.objectId)));
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
    query.addParam("clipId", muse::Val(std::to_string(key.key.objectId)));
    query.addParam("focusItemName", muse::Val("speed"));

    interactive()->open(query);
}

void TrackClipsListModel::resetClipSpeed(const ClipKey& key)
{
    trackeditInteraction()->resetClipSpeed(key.key);
}

QVariant TrackClipsListModel::findGuideline(const ClipKey& key, Direction direction)
{
    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return QVariant();
    }

    TrackClipItem* item = itemByKey(key.key);
    if (!item) {
        return QVariant();
    }

    if (direction != Direction::Right) {
        double clipStartTime = item->time().startTime;
        double guidelineTime = m_context->findGuideline(clipStartTime);
        if (!muse::RealIsEqual(guidelineTime, -1.0)) {
            return QVariant(guidelineTime);
        }
    }
    if (direction != Direction::Left) {
        double clipEndTime = item->time().endTime;
        double guidelineTime = m_context->findGuideline(clipEndTime);
        if (!muse::RealIsEqual(guidelineTime, -1.0)) {
            return QVariant(guidelineTime);
        }
    }

    return QVariant(-1.0);
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

bool ClipsListModel::isContrastFocusBorderEnabled() const
{
    return !uiConfiguration()->isDarkMode();
}

au::projectscene::ClipKey TrackClipsListModel::updateClipTrack(ClipKey clipKey) const
{
    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    if (!prj) {
        return {};
    }

    auto selectedClips = selectionController()->selectedClips();
    for (const auto& selectedClip : selectedClips) {
        if (selectedClip.objectId == clipKey.key.objectId) {
            return selectedClip;
        }
    }

    return clipKey;
}

void TrackClipsListModel::startEditClip(const ClipKey& key)
{
    TrackClipItem* item = itemByKey(key.key);
    if (!item) {
        return;
    }

    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return;
    }

    projectHistory()->startUserInteraction();

    double mousePositionTime = m_context->mousePositionTime();

    vs->setObjectEditStartTimeOffset(mousePositionTime - item->clip().startTime);
    vs->setObjectEditEndTimeOffset(item->clip().endTime - mousePositionTime);
    vs->updateClipsBoundaries(true, key.key);
}

void TrackClipsListModel::endEditClip(const ClipKey& key)
{
    TrackClipItem* item = itemByKey(key.key);
    if (!item) {
        return;
    }

    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return;
    }

    disconnectAutoScroll();

    vs->setObjectEditStartTimeOffset(-1.0);
    vs->setObjectEditEndTimeOffset(-1.0);
    vs->setMoveInitiated(false);
    vs->updateClipsBoundaries(true);

    projectHistory()->endUserInteraction();
}

bool ClipsListModel::cancelClipDragEdit(const ClipKey& key)
{
    ClipListItem* item = itemByKey(key.key);
    if (!item) {
        return false;
    }

    auto vs = globalContext()->currentProject()->viewState();
    IF_ASSERT_FAILED(vs) {
        return false;
    }

    vs->setObjectEditStartTimeOffset(-1.0);
    vs->setObjectEditEndTimeOffset(-1.0);
    vs->setMoveInitiated(false);

    m_context->stopAutoScroll();
    disconnectAutoScroll();

    trackeditInteraction()->cancelClipDragEdit();

    vs->updateClipsBoundaries(true);

    constexpr auto modifyState = false;
    projectHistory()->endUserInteraction(modifyState);

    return true;
}

/*!
 * \brief Moves all selected clips
 * \param key - the key from which the offset will be calculated to move all clips

    Calculate offset of clip that's being grabbed
    and apply it to all selected clips
 */
bool TrackClipsListModel::moveSelectedClips(const ClipKey& key, bool completed)
{
    TrackClipItem* item = itemByKey(key.key);
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
    MoveOffset moveOffset = calculateMoveOffset(item, key, completed);
    if (vs->moveInitiated()) {
        trackeditInteraction()->moveClips(moveOffset.timeOffset, moveOffset.trackOffset, completed, clipsMovedToOtherTrack);
    }

    if ((completed && m_autoScrollConnection)) {
        disconnectAutoScroll();
    } else if (!m_autoScrollConnection && !completed) {
        m_autoScrollConnection = connect(m_context, &TimelineContext::frameTimeChanged, [this, key](){ moveSelectedClips(key, false); });
    }

    return clipsMovedToOtherTrack;
}

bool TrackClipsListModel::trimLeftClip(const ClipKey& key, bool completed, ClipBoundary::Action action)
{
    TrackClipItem* item = itemByKey(key.key);
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

        newStartTime = item->clip().startTime + offset;

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
        newStartTime = m_context->mousePositionTime() - vs->objectEditStartTimeOffset();
        if (vs->isSnapEnabled()) {
            newStartTime = m_context->applySnapToTime(newStartTime);
        } else {
            newStartTime = m_context->applySnapToClip(newStartTime);
        }
    }

    double minClipTime = MIN_CLIP_WIDTH / m_context->zoom();
    double newClipTime = item->clip().endTime - newStartTime;
    if (newClipTime < minClipTime) {
        newStartTime = item->clip().endTime - minClipTime;
    }

    newStartTime = std::max(newStartTime, 0.0);

    bool ok = trackeditInteraction()->trimClipLeft(key.key, newStartTime - item->clip().startTime, minClipTime, completed, undoType);

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
    TrackClipItem* item = itemByKey(key.key);
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

        newEndTime = item->clip().endTime - offset;

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
        newEndTime = m_context->mousePositionTime() + vs->objectEditEndTimeOffset();
        if (vs->isSnapEnabled()) {
            newEndTime = m_context->applySnapToTime(newEndTime);
        } else {
            newEndTime = m_context->applySnapToClip(newEndTime);
        }
    }

    double minClipTime = MIN_CLIP_WIDTH / m_context->zoom();
    double newClipTime = newEndTime - item->clip().startTime;
    if (newClipTime < minClipTime) {
        newEndTime = item->clip().startTime + minClipTime;
    }

    bool ok = trackeditInteraction()->trimClipRight(key.key, item->clip().endTime - newEndTime, minClipTime, completed, undoType);

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
    TrackClipItem* item = itemByKey(key.key);
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

        newStartTime = item->clip().startTime + offset;

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
        newStartTime = m_context->mousePositionTime() - vs->objectEditStartTimeOffset();
        if (vs->isSnapEnabled()) {
            newStartTime = m_context->applySnapToTime(newStartTime);
        } else {
            newStartTime = m_context->applySnapToClip(newStartTime);
        }
    }

    double minClipTime = MIN_CLIP_WIDTH / m_context->zoom();
    double newClipTime = item->clip().endTime - newStartTime;
    if (newClipTime < minClipTime) {
        newStartTime = item->clip().endTime - minClipTime;
    }

    newStartTime = std::max(newStartTime, 0.0);

    bool ok = trackeditInteraction()->stretchClipLeft(key.key, newStartTime - item->clip().startTime, minClipTime, completed, undoType);

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
    TrackClipItem* item = itemByKey(key.key);
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

        newEndTime = item->clip().endTime - offset;

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
        newEndTime = m_context->mousePositionTime() + vs->objectEditEndTimeOffset();
        if (vs->isSnapEnabled()) {
            newEndTime = m_context->applySnapToTime(newEndTime);
        } else {
            newEndTime = m_context->applySnapToClip(newEndTime);
        }
    }

    double minClipTime = MIN_CLIP_WIDTH / m_context->zoom();
    double newClipTime = newEndTime - item->clip().startTime;
    if (newClipTime < minClipTime) {
        newEndTime = item->clip().startTime + minClipTime;
    }

    bool ok = trackeditInteraction()->stretchClipRight(key.key, item->clip().endTime - newEndTime, minClipTime, completed, undoType);

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
            selectionController()->setSelectedClips(ClipKeyList({ key.key }), complete);
        }
    }
}

void TrackClipsListModel::resetSelectedClips()
{
    clearSelectedItems();
    selectionController()->resetSelectedClips();
}

void TrackClipsListModel::requestClipTitleChange()
{
    auto selectedClips = selectionController()->selectedClips();

    if (selectedClips.empty() || selectedClips.size() > 1) {
        return;
    }

    trackedit::ClipKey clipKey = selectedClips.front();
    if (!clipKey.isValid()) {
        return;
    }

    TrackClipItem* selectedItem = itemByKey(clipKey);
    if (selectedItem != nullptr) {
        emit selectedItem->titleEditRequested();
    }
}

void TrackClipsListModel::onSelectedClip(const trackedit::ClipKey& k)
{
    // ignore if item already selected
    for (const auto& selectedItem : m_selectedItems) {
        if (selectedItem->clip().key == k) {
            return;
        }
    }

    Qt::KeyboardModifiers modifiers = keyboardModifiers();

    auto item = itemByKey(k);
    if (modifiers.testFlag(Qt::ShiftModifier)) {
        if (m_trackId != k.trackId) {
            return;
        } else {
            if (item) {
                addSelectedItem(item);
            }
        }
    } else {
        if (m_trackId != k.trackId) {
            clearSelectedItems();
        } else {
            if (item) {
                setSelectedItems(QList<TrackClipItem*>({ item }));
            }
        }
    }
}

void TrackClipsListModel::onSelectedClips(const trackedit::ClipKeyList& keyList)
{
    if (keyList.size() == 1) {
        onSelectedClip(keyList.front());
        return;
    }

    // Multiple-clip selection can only be done programmatically, hence there is no need to check for the Shift key ;
    // we can begin by clearing everything.
    clearSelectedItems();

    QList<TrackClipItem*> items;
    for (const auto& k : keyList) {
        if (const auto item = itemByKey(k)) {
            items.append(item);
        }
    }
    setSelectedItems(items);
}

QVariant TrackClipsListModel::trackId() const
{
    return QVariant::fromValue(m_trackId);
}

void TrackClipsListModel::setTrackId(const QVariant& _newTrackId)
{
    trackedit::TrackId newTrackId = _newTrackId.toInt();
    if (m_trackId == newTrackId) {
        return;
    }
    m_trackId = newTrackId;
    emit trackIdChanged();
}

bool TrackClipsListModel::isStereo() const
{
    return m_isStereo;
}

ClipStyles::Style TrackClipsListModel::clipStyle() const
{
    return projectSceneConfiguration()->clipStyle();
}
TimelineContext* TrackClipsListModel::timelineContext() const
{
    return m_context;
}

void TrackClipsListModel::setTimelineContext(TimelineContext* newContext)
{
    if (m_context == newContext) {
        return;
    }

    if (m_context) {
        disconnect(m_context, nullptr, this, nullptr);
    }

    m_context = newContext;

    if (m_context) {
        connect(m_context, &TimelineContext::zoomChanged, this, &TrackClipsListModel::onTimelineZoomChanged);
        connect(m_context, &TimelineContext::frameTimeChanged, this, &TrackClipsListModel::onTimelineFrameTimeChanged);
    }

    emit timelineContextChanged();
}

int TrackClipsListModel::cacheBufferPx()
{
    return CACHE_BUFFER_PX;
}

