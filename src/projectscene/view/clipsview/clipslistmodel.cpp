/*
* Audacity: A Digital Audio Editor
*/
#include "clipslistmodel.h"

#include "global/realfn.h"
#include "global/async/async.h"

#include "types/projectscenetypes.h"

#include "log.h"

using namespace au::projectscene;
using namespace au::trackedit;

constexpr int CACHE_BUFFER_PX = 200;
constexpr double MOVE_MAX = 100000.0;
constexpr double MOVE_MIN = 0.0;
constexpr double MIN_CLIP_WIDTH = 3.0;

static const muse::Uri EDIT_PITCH_AND_SPEED_URI("audacity://projectscene/editpitchandspeed");

ClipsListModel::ClipsListModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

ClipsListModel::~ClipsListModel()
{
    if (m_autoScrollConnection) {
        disconnect(m_autoScrollConnection);
    }
}

void ClipsListModel::init()
{
    IF_ASSERT_FAILED(m_trackId >= 0) {
        return;
    }

    dispatcher()->reg(this, "clip-rename", this, &ClipsListModel::onClipRenameAction);

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

    reload();
}

void ClipsListModel::reload()
{
    ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    prj->trackChanged().onReceive(this, [this](const au::trackedit::Track& track) {
        if (track.id == m_trackId) {
            reload();
        }
    });

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
    });

    m_allClipList.onItemChanged(this, [this](const Clip& clip) {
        for (size_t i = 0; i < m_allClipList.size(); ++i) {
            if (m_allClipList.at(i).key != clip.key) {
                continue;
            }
            m_allClipList[i] = clip;
            break;
        }

        // LOGDA() << "clip: " << clip.key << ", startTime: " << clip.startTime;
        ClipListItem* item = itemByKey(clip.key);
        if (item) {
            item->setClip(clip);
        }

        updateItemsMetrics();
    });

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
    });

    m_allClipList.onItemRemoved(this, [this](const Clip& clip) {
        for (auto it = m_allClipList.begin(); it != m_allClipList.end(); ++it) {
            if (it->key == clip.key) {
                m_allClipList.erase(it);
                update();
                break;
            }
        }
    });

    update();
}

ClipListItem* ClipsListModel::itemByKey(const trackedit::ClipKey& k) const
{
    for (ClipListItem* item : std::as_const(m_clipList)) {
        if (item->clip().key != k) {
            continue;
        }
        return item;
    }
    return nullptr;
}

int ClipsListModel::indexByKey(const trackedit::ClipKey& k) const
{
    for (int i = 0; i < m_clipList.size(); ++i) {
        if (m_clipList.at(i)->clip().key == k) {
            return i;
        }
    }
    return -1;
}

Qt::KeyboardModifiers ClipsListModel::keyboardModifiers() const
{
    Qt::KeyboardModifiers modifiers = QApplication::keyboardModifiers();

    //! NOTE: always treat simultaneously pressed Ctrl and Shift as Ctrl
    if (modifiers.testFlag(Qt::ShiftModifier) && modifiers.testFlag(Qt::ControlModifier)) {
        modifiers = Qt::ControlModifier;
    }

    return modifiers;
}

void ClipsListModel::update()
{
    //! NOTE First we form a new list, and then we delete old objects,
    //! otherwise there will be errors in Qml
    QList<ClipListItem*> oldList = m_clipList;
    bool isStereo = false;
    beginResetModel();

    m_clipList.clear();

    for (const au::trackedit::Clip& c : m_allClipList) {
        ClipListItem* item = new ClipListItem(this);
        item->setClip(c);
        m_clipList.append(item);
        isStereo |= c.stereo;
    }

    std::sort(m_clipList.begin(), m_clipList.end(), [](ClipListItem* a, ClipListItem* b) {
        return a->time().clipStartTime < b->time().clipStartTime;
    });

    updateItemsMetrics();

    //! NOTE We need to update the selected items
    //! to take pointers to the items from the new list
    m_selectedItems.clear();
    onSelectedClips(selectionController()->selectedClips());
    m_context->updateSelectedClipTime();

    endResetModel();

    if (m_isStereo != isStereo) {
        m_isStereo = isStereo;
        emit isStereoChanged();
    }

    muse::async::Async::call(this, [oldList]() {
        qDeleteAll(oldList);
    });
}

void ClipsListModel::updateItemsMetrics()
{
    for (int i = 0; i < m_clipList.size(); ++i) {
        ClipListItem* item = m_clipList[i];
        updateItemsMetrics(item);
    }
}

void ClipsListModel::updateItemsMetrics(ClipListItem* item)
{
    //! NOTE The first step is to calculate the position and width
    const double cacheTime = CACHE_BUFFER_PX / m_context->zoom();

    const trackedit::Clip& clip = item->clip();

    ClipTime time;
    time.clipStartTime = clip.startTime;
    time.clipEndTime = clip.endTime;
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

void ClipsListModel::positionViewAtClip(const Clip& clip)
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

int ClipsListModel::rowCount(const QModelIndex&) const
{
    return static_cast<int>(m_clipList.size());
}

QHash<int, QByteArray> ClipsListModel::roleNames() const
{
    static QHash<int, QByteArray> roles
    {
        { ClipItemRole, "item" }
    };
    return roles;
}

QVariant ClipsListModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid()) {
        return QVariant();
    }

    switch (role) {
    case ClipItemRole: {
        ClipListItem* item = m_clipList.at(index.row());
        return QVariant::fromValue(item);
    }
    default:
        break;
    }

    return QVariant();
}

void ClipsListModel::onTimelineZoomChanged()
{
    updateItemsMetrics();
}

void ClipsListModel::onTimelineFrameTimeChanged()
{
    updateItemsMetrics();
}

void ClipsListModel::setSelectedItems(const QList<ClipListItem*>& items)
{
    for (auto& selectedItem : m_selectedItems) {
        selectedItem->setSelected(false);
    }
    m_selectedItems = items;
    for (auto& selectedItem : m_selectedItems) {
        selectedItem->setSelected(true);
    }
}

void ClipsListModel::addSelectedItem(ClipListItem* item)
{
    item->setSelected(true);
    m_selectedItems.append(item);
}

void ClipsListModel::clearSelectedItems()
{
    for (auto& selectedItem : m_selectedItems) {
        selectedItem->setSelected(false);
    }
    m_selectedItems.clear();
}

void ClipsListModel::onClipRenameAction(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() > 0) {
        return;
    }

    trackedit::ClipKey key = args.arg<trackedit::ClipKey>(0);
    int idx = indexByKey(key);

    IF_ASSERT_FAILED(idx != -1) {
        return;
    }

    emit requestClipTitleEdit(idx);
}

bool ClipsListModel::changeClipTitle(const ClipKey& key, const QString& newTitle)
{
    bool ok = trackeditInteraction()->changeClipTitle(key.key, newTitle);
    return ok;
}

QVariant ClipsListModel::next(const ClipKey& key) const
{
    return neighbor(key, 1);
}

QVariant ClipsListModel::prev(const ClipKey& key) const
{
    return neighbor(key, -1);
}

QVariant ClipsListModel::neighbor(const ClipKey& key, int offset) const
{
    auto it = std::find_if(m_clipList.begin(), m_clipList.end(), [key](ClipListItem* clip) {
        return clip->key().key.clipId == key.key.clipId;
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

int ClipsListModel::calculateTrackPositionOffset(const ClipKey& key, bool completed) const
{
    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    if (!prj) {
        return 0;
    }

    if (completed) {
        return 0;
    }

    auto vs = prj->viewState();
    auto yPos = vs->mousePositionY();

    auto tracks = selectionController()->determinateTracks(vs->trackYPosition(key.key.trackId) + 2, yPos);
    auto pointedTrack = selectionController()->determinePointedTrack(yPos);
    if (!pointedTrack.has_value()) {
        return 0;
    }
    bool pointingAtEmptySpace = (pointedTrack == INVALID_TRACK);
    int trackPositionOffset = pointingAtEmptySpace ? tracks.size() : tracks.size() - 1;

    if (!muse::RealIsEqualOrMore(yPos, vs->trackYPosition(key.key.trackId))) {
        trackPositionOffset = -trackPositionOffset;
    }

    return trackPositionOffset;
}

secs_t ClipsListModel::calculateTimePositionOffset(const ClipListItem* item) const
{
    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return 0.0;
    }

    double newStartTime = m_context->mousePositionTime() - vs->clipEditStartTimeOffset();
    newStartTime = m_context->applySnapToTime(newStartTime);
    secs_t timePositionOffset = newStartTime - item->time().clipStartTime;

    constexpr auto limit = 1. / 192000.; // 1 sample at 192 kHz
    if (!muse::RealIsEqualOrMore(std::abs(timePositionOffset), limit)) {
        timePositionOffset = 0.0;
    }

    return timePositionOffset;
}

void ClipsListModel::openClipPitchEdit(const ClipKey& key)
{
    selectClip(key);

    if (interactive()->isOpened(EDIT_PITCH_AND_SPEED_URI).val) {
        return;
    }

    muse::UriQuery query(EDIT_PITCH_AND_SPEED_URI);
    query.addParam("trackId", muse::Val(std::to_string(key.key.trackId)));
    query.addParam("clipId", muse::Val(std::to_string(key.key.clipId)));
    query.addParam("focusItemName", muse::Val("pitch"));

    interactive()->open(query);
}

void ClipsListModel::resetClipPitch(const ClipKey& key)
{
    trackeditInteraction()->resetClipPitch(key.key);
}

void ClipsListModel::openClipSpeedEdit(const ClipKey& key)
{
    selectClip(key);

    if (interactive()->isOpened(EDIT_PITCH_AND_SPEED_URI).val) {
        return;
    }

    muse::UriQuery query(EDIT_PITCH_AND_SPEED_URI);
    query.addParam("trackId", muse::Val(std::to_string(key.key.trackId)));
    query.addParam("clipId", muse::Val(std::to_string(key.key.clipId)));
    query.addParam("focusItemName", muse::Val("speed"));

    interactive()->open(query);
}

void ClipsListModel::resetClipSpeed(const ClipKey& key)
{
    trackeditInteraction()->resetClipSpeed(key.key);
}

au::projectscene::ClipKey ClipsListModel::updateClipTrack(ClipKey clipKey) const
{
    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    if (!prj) {
        return {};
    }

    auto selectedClips = selectionController()->selectedClips();
    for (const auto& selectedClip : selectedClips) {
        if (selectedClip.clipId == clipKey.key.clipId) {
            return selectedClip;
        }
    }

    return clipKey;
}

void ClipsListModel::startEditClip(const ClipKey& key)
{
    ClipListItem* item = itemByKey(key.key);
    if (!item) {
        return;
    }

    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return;
    }

    double mousePositionTime = m_context->mousePositionTime();

    vs->setClipEditStartTimeOffset(mousePositionTime - item->clip().startTime);
    vs->setClipEditEndTimeOffset(item->clip().endTime - mousePositionTime);
}

void ClipsListModel::endEditClip(const ClipKey& key)
{
    UNUSED(key);

    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return;
    }

    vs->setClipEditStartTimeOffset(-1.0);
    vs->setClipEditEndTimeOffset(-1.0);
}

/*!
 * \brief Moves all selected clips
 * \param key - the key from which the offset will be calculated to move all clips

    Calculate offset of clip that's being grabbed
    and apply it to all selected clips
 */
bool ClipsListModel::moveSelectedClips(const ClipKey& key, bool completed)
{
    ClipListItem* item = itemByKey(key.key);
    if (!item) {
        return false;
    }

    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return false;
    }

    secs_t timePositionOffset = calculateTimePositionOffset(item);
    int trackPositionOffset = calculateTrackPositionOffset(key, completed);

    bool clipsMovedToOtherTrack = trackeditInteraction()->moveClips(timePositionOffset, trackPositionOffset, completed);

    m_context->updateSelectedClipTime();

    if ((completed && m_autoScrollConnection)) {
        disconnect(m_autoScrollConnection);
    } else if (!m_autoScrollConnection && !completed) {
        m_autoScrollConnection = connect(m_context, &TimelineContext::frameTimeChanged, [this, key](){ moveSelectedClips(key, false); });
    }

    return clipsMovedToOtherTrack;
}

bool ClipsListModel::trimLeftClip(const ClipKey& key, bool completed)
{
    ClipListItem* item = itemByKey(key.key);
    IF_ASSERT_FAILED(item) {
        return false;
    }

    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return false;
    }

    double newStartTime = m_context->mousePositionTime() - vs->clipEditStartTimeOffset();

    newStartTime = m_context->applySnapToTime(newStartTime);

    double minClipTime = MIN_CLIP_WIDTH / m_context->zoom();
    double newClipTime = item->clip().endTime - newStartTime;
    if (newClipTime < minClipTime) {
        newStartTime = item->clip().endTime - minClipTime;
    }

    newStartTime = std::max(newStartTime, 0.0);

    if (muse::RealIsEqual(newStartTime, item->clip().startTime)) {
        return false;
    }

    bool ok = trackeditInteraction()->trimClipLeft(key.key, newStartTime - item->clip().startTime, minClipTime, completed);
    m_context->updateSelectedClipTime();

    // handle auto-scroll over the edge
    if (!ok) {
        m_context->stopAutoScroll();
    } else {
        m_context->startAutoScroll(m_context->mousePositionTime());
    }

    if ((completed && m_autoScrollConnection) || !ok) {
        disconnect(m_autoScrollConnection);
    } else if (!m_autoScrollConnection && !completed) {
        m_autoScrollConnection = connect(m_context, &TimelineContext::frameTimeChanged, [this, key](){ trimLeftClip(key, false); });
    }

    return ok;
}

bool ClipsListModel::trimRightClip(const ClipKey& key, bool completed)
{
    ClipListItem* item = itemByKey(key.key);
    IF_ASSERT_FAILED(item) {
        return false;
    }

    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return false;
    }

    double newEndTime = m_context->mousePositionTime() + vs->clipEditEndTimeOffset();

    newEndTime = m_context->applySnapToTime(newEndTime);

    double minClipTime = MIN_CLIP_WIDTH / m_context->zoom();
    double newClipTime = newEndTime - item->clip().startTime;
    if (newClipTime < minClipTime) {
        newEndTime = item->clip().startTime + minClipTime;
    }

    if (muse::RealIsEqual(newEndTime, item->clip().endTime)) {
        return false;
    }

    bool ok = trackeditInteraction()->trimClipRight(key.key, item->clip().endTime - newEndTime, minClipTime, completed);
    m_context->updateSelectedClipTime();

    // handle auto-scroll over the edge
    if (!ok) {
        m_context->stopAutoScroll();
    } else {
        m_context->startAutoScroll(m_context->mousePositionTime());
    }

    if ((completed && m_autoScrollConnection) || !ok) {
        disconnect(m_autoScrollConnection);
    } else if (!m_autoScrollConnection && !completed) {
        m_autoScrollConnection = connect(m_context, &TimelineContext::frameTimeChanged, [this, key](){ trimRightClip(key, false); });
    }

    return ok;
}

bool ClipsListModel::stretchLeftClip(const ClipKey& key, bool completed)
{
    ClipListItem* item = itemByKey(key.key);
    IF_ASSERT_FAILED(item) {
        return false;
    }

    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return false;
    }

    double newStartTime = m_context->mousePositionTime() - vs->clipEditStartTimeOffset();

    newStartTime = m_context->applySnapToTime(newStartTime);

    double minClipTime = MIN_CLIP_WIDTH / m_context->zoom();
    double newClipTime = item->clip().endTime - newStartTime;
    if (newClipTime < minClipTime) {
        newStartTime = item->clip().endTime - minClipTime;
    }

    newStartTime = std::max(newStartTime, 0.0);

    if (muse::RealIsEqual(newStartTime, item->clip().startTime)) {
        return false;
    }

    bool ok = trackeditInteraction()->stretchClipLeft(key.key, newStartTime - item->clip().startTime, minClipTime, completed);
    m_context->updateSelectedClipTime();

    // handle auto-scroll over the edge
    if (!ok) {
        m_context->stopAutoScroll();
    } else {
        m_context->startAutoScroll(m_context->mousePositionTime());
    }

    if ((completed && m_autoScrollConnection) || !ok) {
        disconnect(m_autoScrollConnection);
    } else if (!m_autoScrollConnection && !completed) {
        m_autoScrollConnection = connect(m_context, &TimelineContext::frameTimeChanged, [this, key](){ stretchLeftClip(key, false); });
    }

    return ok;
}

bool ClipsListModel::stretchRightClip(const ClipKey& key, bool completed)
{
    ClipListItem* item = itemByKey(key.key);
    IF_ASSERT_FAILED(item) {
        return false;
    }

    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return false;
    }

    double newEndTime = m_context->mousePositionTime() + vs->clipEditEndTimeOffset();

    newEndTime = m_context->applySnapToTime(newEndTime);

    double minClipTime = MIN_CLIP_WIDTH / m_context->zoom();
    double newClipTime = newEndTime - item->clip().startTime;
    if (newClipTime < minClipTime) {
        newEndTime = item->clip().startTime + minClipTime;
    }

    if (muse::RealIsEqual(newEndTime, item->clip().endTime)) {
        return false;
    }

    bool ok = trackeditInteraction()->stretchClipRight(key.key, item->clip().endTime - newEndTime, minClipTime, completed);
    m_context->updateSelectedClipTime();

    // handle auto-scroll over the edge
    if (!ok) {
        m_context->stopAutoScroll();
    } else {
        m_context->startAutoScroll(m_context->mousePositionTime());
    }

    if ((completed && m_autoScrollConnection) || !ok) {
        disconnect(m_autoScrollConnection);
    } else if (!m_autoScrollConnection && !completed) {
        m_autoScrollConnection = connect(m_context, &TimelineContext::frameTimeChanged, [this, key](){ stretchRightClip(key, false); });
    }

    return ok;
}

void ClipsListModel::selectClip(const ClipKey& key)
{
    Qt::KeyboardModifiers modifiers = keyboardModifiers();

    const auto clipGroupId = trackeditInteraction()->clipGroupId(key.key);
    if (clipGroupId != -1) {
        //! NOTE: clip belongs to a group, select the whole group
        if (modifiers.testFlag(Qt::ShiftModifier)) {
            for (const auto& key : trackeditInteraction()->clipsInGroup(clipGroupId)) {
                selectionController()->addSelectedClip(key);
            }
        } else {
            selectionController()->setSelectedClips(trackeditInteraction()->clipsInGroup(clipGroupId));
        }
    } else {
        if (modifiers.testFlag(Qt::ShiftModifier)) {
            selectionController()->addSelectedClip(key.key);
        } else {
            if (muse::contains(selectionController()->selectedClips(), key.key)) {
                return;
            }
            selectionController()->setSelectedClips(ClipKeyList({ key.key }));
        }
    }
}

void ClipsListModel::resetSelectedClips()
{
    selectionController()->resetSelectedClips();
    clearSelectedItems();
}

void ClipsListModel::onSelectedClip(const trackedit::ClipKey& k)
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
                setSelectedItems(QList<ClipListItem*>({ item }));
            }
        }
    }
}

void ClipsListModel::onSelectedClips(const trackedit::ClipKeyList& keyList)
{
    if (keyList.size() == 1) {
        onSelectedClip(keyList.front());
        return;
    }

    // Multiple-clip selection can only be done programmatically, hence there is no need to check for the Shift key ;
    // we can begin by clearing everything.
    clearSelectedItems();

    QList<ClipListItem*> items;
    for (const auto& k : keyList) {
        if (const auto item = itemByKey(k)) {
            items.append(item);
        }
    }
    setSelectedItems(items);
}

QVariant ClipsListModel::trackId() const
{
    return QVariant::fromValue(m_trackId);
}

void ClipsListModel::setTrackId(const QVariant& _newTrackId)
{
    trackedit::TrackId newTrackId = _newTrackId.toInt();
    if (m_trackId == newTrackId) {
        return;
    }
    m_trackId = newTrackId;
    emit trackIdChanged();
}

bool ClipsListModel::isStereo() const
{
    return m_isStereo;
}

TimelineContext* ClipsListModel::timelineContext() const
{
    return m_context;
}

void ClipsListModel::setTimelineContext(TimelineContext* newContext)
{
    if (m_context == newContext) {
        return;
    }

    if (m_context) {
        disconnect(m_context, nullptr, this, nullptr);
    }

    m_context = newContext;

    if (m_context) {
        connect(m_context, &TimelineContext::zoomChanged, this, &ClipsListModel::onTimelineZoomChanged);
        connect(m_context, &TimelineContext::frameTimeChanged, this, &ClipsListModel::onTimelineFrameTimeChanged);
    }

    emit timelineContextChanged();
}

int ClipsListModel::cacheBufferPx() const
{
    return CACHE_BUFFER_PX;
}
