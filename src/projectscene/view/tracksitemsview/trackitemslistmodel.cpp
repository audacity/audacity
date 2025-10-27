/*
* Audacity: A Digital Audio Editor
*/
#include "trackitemslistmodel.h"

#include <QApplication>

#include "global/realfn.h"

using namespace au::projectscene;
using namespace au::trackedit;

constexpr int CACHE_BUFFER_PX = 200;
constexpr double MOVE_THRESHOLD = 3.0;

TrackItemsListModel::TrackItemsListModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

TrackItemsListModel::~TrackItemsListModel()
{
    disconnectAutoScroll();
}

QVariant TrackItemsListModel::trackId() const
{
    return QVariant::fromValue(m_trackId);
}

void TrackItemsListModel::setTrackId(const QVariant& _newTrackId)
{
    trackedit::TrackId newTrackId = _newTrackId.toInt();
    if (m_trackId == newTrackId) {
        return;
    }
    m_trackId = newTrackId;
    emit trackIdChanged();
}

TimelineContext* TrackItemsListModel::timelineContext() const
{
    return m_context;
}

void TrackItemsListModel::setTimelineContext(TimelineContext* newContext)
{
    if (m_context == newContext) {
        return;
    }

    if (m_context) {
        disconnect(m_context, nullptr, this, nullptr);
    }

    m_context = newContext;

    if (m_context) {
        connect(m_context, &TimelineContext::zoomChanged, this, &TrackItemsListModel::onTimelineZoomChanged);
        connect(m_context, &TimelineContext::frameTimeChanged, this, &TrackItemsListModel::onTimelineFrameTimeChanged);
    }

    emit timelineContextChanged();
}

void TrackItemsListModel::onTimelineZoomChanged()
{
    updateItemsMetrics();
}

void TrackItemsListModel::onTimelineFrameTimeChanged()
{
    updateItemsMetrics();
}

void TrackItemsListModel::updateItemsMetrics()
{
    for (int i = 0; i < m_items.size(); ++i) {
        updateItemMetrics(m_items[i]);
    }
}

void TrackItemsListModel::setSelectedItems(const QList<ViewTrackItem*>& items)
{
    for (auto& selectedItem : m_selectedItems) {
        selectedItem->setSelected(false);
    }
    m_selectedItems = items;
    for (auto& selectedItem : m_selectedItems) {
        selectedItem->setSelected(true);
    }
}

void TrackItemsListModel::addSelectedItem(ViewTrackItem* item)
{
    item->setSelected(true);
    m_selectedItems.append(item);
}

void TrackItemsListModel::clearSelectedItems()
{
    for (auto& selectedItem : m_selectedItems) {
        selectedItem->setSelected(false);
    }
    m_selectedItems.clear();
}

ViewTrackItem* TrackItemsListModel::itemByKey(const trackedit::TrackItemKey& key) const
{
    for (ViewTrackItem* item : std::as_const(m_items)) {
        if (item->key().key != key) {
            continue;
        }
        return item;
    }
    return nullptr;
}

int TrackItemsListModel::indexByKey(const trackedit::TrackItemKey& key) const
{
    for (int i = 0; i < m_items.size(); ++i) {
        if (m_items.at(i)->key().key == key) {
            return i;
        }
    }
    return -1;
}

void TrackItemsListModel::onSelectedItem(const trackedit::TrackItemKey& k)
{
    // ignore if item already selected
    for (const auto& selectedItem : m_selectedItems) {
        if (selectedItem->key().key == k) {
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
                setSelectedItems(QList<ViewTrackItem*>({ item }));
            }
        }
    }
}

void TrackItemsListModel::onSelectedItems(const trackedit::TrackItemKeyList& keyList)
{
    if (keyList.size() == 1) {
        onSelectedItem(keyList.front());
        return;
    }

    // Multiple-item selection can only be done programmatically, hence there is no need to check for the Shift key ;
    // we can begin by clearing everything.
    clearSelectedItems();

    QList<ViewTrackItem*> items;
    for (const auto& k : keyList) {
        if (const auto item = itemByKey(k)) {
            items.append(item);
        }
    }
    setSelectedItems(items);
}

QVariant TrackItemsListModel::next(const TrackItemKey& key) const
{
    return neighbor(key, 1);
}

QVariant TrackItemsListModel::prev(const TrackItemKey& key) const
{
    return neighbor(key, -1);
}

QVariant TrackItemsListModel::neighbor(const TrackItemKey& key, int offset) const
{
    auto it = std::find_if(m_items.begin(), m_items.end(), [key](ViewTrackItem* viewItem) {
        return viewItem->key().key.itemId == key.key.itemId;
    });

    if (it == m_items.end()) {
        return QVariant();
    }

    int sortedIndex = std::distance(m_items.begin(), it) + offset;
    if (sortedIndex < 0 || sortedIndex >= m_items.size()) {
        return QVariant();
    }

    return QVariant::fromValue(m_items[sortedIndex]);
}

TrackItemsListModel::MoveOffset TrackItemsListModel::calculateMoveOffset(const ViewTrackItem* item,
                                                                         const TrackItemKey& key,
                                                                         const std::vector<trackedit::TrackType>& trackTypesAllowedToMove,
                                                                         bool completed)
const
{
    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    if (!prj) {
        return MoveOffset{};
    }

    auto vs = prj->viewState();

    MoveOffset moveOffset {
        calculateTimePositionOffset(item),
        completed ? 0 : calculateTrackPositionOffset(key, trackTypesAllowedToMove)
    };

    secs_t positionOffsetX = moveOffset.timeOffset * m_context->zoom();
    if (!vs->moveInitiated() && (muse::RealIsEqualOrMore(std::abs(positionOffsetX), MOVE_THRESHOLD) || moveOffset.trackOffset != 0)) {
        vs->setMoveInitiated(true);
    } else if (!vs->moveInitiated()) {
        moveOffset.timeOffset = 0.0;
    }

    return moveOffset;
}

int TrackItemsListModel::calculateTrackPositionOffset(const TrackItemKey& key,
                                                      const std::vector<trackedit::TrackType>& trackTypesAllowedToMove) const
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

    // Check if mouse is pointing at a track with allowed type
    if (!trackTypesAllowedToMove.empty()) {
        trackedit::TrackId targetTrackId = vs->trackAtPosition(yPos);
        if (targetTrackId != trackedit::INVALID_TRACK) {
            if (!isAllowedToMoveToTracks(trackTypesAllowedToMove, targetTrackId)) {
                return 0;
            }
        }
    }

    // Calculate offset based on allowed track types only
    trackedit::TrackId targetTrackId = vs->trackAtPosition(yPos);
    bool pointingAtEmptySpace = yPos > vs->totalTrackHeight().val - vs->tracksVerticalOffset().val;

    if (targetTrackId == trackedit::INVALID_TRACK && !pointingAtEmptySpace) {
        return 0;
    }

    ITrackeditProjectPtr trackeditPrj = globalContext()->currentTrackeditProject();
    if (!trackeditPrj) {
        return 0;
    }

    TrackIdList allTracks = trackeditPrj->trackIdList();

    int sourceAllowedIndex = -1;
    int targetAllowedIndex = -1;
    int allowedCount = 0;

    for (size_t i = 0; i < allTracks.size(); ++i) {
        auto track = trackeditPrj->track(allTracks[i]);
        if (!track.has_value()) {
            continue;
        }

        if (!muse::contains(trackTypesAllowedToMove, track->type)) {
            continue;
        }

        if (allTracks[i] == key.key.trackId) {
            sourceAllowedIndex = allowedCount;
        }

        if (allTracks[i] == targetTrackId) {
            targetAllowedIndex = allowedCount;
        }

        allowedCount++;
    }

    int trackPositionOffset = 0;
    if (pointingAtEmptySpace) {
        if (sourceAllowedIndex >= 0) {
            trackPositionOffset = allowedCount - sourceAllowedIndex;
        }
    } else if (sourceAllowedIndex >= 0 && targetAllowedIndex >= 0) {
        trackPositionOffset = targetAllowedIndex - sourceAllowedIndex;
    }

    return trackPositionOffset;
}

bool TrackItemsListModel::isAllowedToMoveToTracks(const std::vector<trackedit::TrackType>& allowedTrackTypes,
                                                  const trackedit::TrackId& movedTrackId) const
{
    ITrackeditProjectPtr trackeditPrj = globalContext()->currentTrackeditProject();
    if (!trackeditPrj) {
        return true;
    }

    auto track = trackeditPrj->track(movedTrackId);
    return track.has_value() ? muse::contains(allowedTrackTypes, track->type) : false;
}

secs_t TrackItemsListModel::calculateTimePositionOffset(const ViewTrackItem* item) const
{
    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return 0.0;
    }

    double newStartTime = m_context->mousePositionTime() - vs->itemEditStartTimeOffset();
    double duration = item->time().endTime - item->time().startTime;
    double newEndTime = newStartTime + duration;

    double snappedEndTime = newEndTime;
    double snappedStartTime = newStartTime;
    if (vs->isSnapEnabled()) {
        snappedStartTime = m_context->applySnapToTime(newStartTime);
    } else {
        snappedEndTime = m_context->applySnapToItem(newEndTime);
        snappedStartTime = m_context->applySnapToItem(newStartTime);
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

void TrackItemsListModel::requestItemTitleChange()
{
    auto selectedItems = getSelectedItemKeys();

    if (selectedItems.empty() || selectedItems.size() > 1) {
        return;
    }

    trackedit::TrackItemKey itemKey = selectedItems.front();
    if (!itemKey.isValid()) {
        return;
    }

    ViewTrackItem* selectedItem = itemByKey(itemKey);
    if (selectedItem != nullptr) {
        emit selectedItem->titleEditRequested();
    }
}

int TrackItemsListModel::rowCount(const QModelIndex&) const
{
    return static_cast<int>(m_items.size());
}

QHash<int, QByteArray> TrackItemsListModel::roleNames() const
{
    static QHash<int, QByteArray> roles
    {
        { ItemRole, "item" }
    };
    return roles;
}

QVariant TrackItemsListModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid()) {
        return QVariant();
    }

    switch (role) {
    case ItemRole: {
        ViewTrackItem* item = m_items.at(index.row());
        return QVariant::fromValue(item);
    }
    default:
        break;
    }

    return QVariant();
}

void TrackItemsListModel::handleAutoScroll(bool ok, bool completed, const std::function<void()>& onAutoScrollFrame)
{
    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return;
    }

    // do not handle auto-scroll when using key-nav
    if (muse::RealIsEqual(vs->itemEditStartTimeOffset(), -1.0)) {
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

void TrackItemsListModel::disconnectAutoScroll()
{
    if (m_autoScrollConnection) {
        disconnect(m_autoScrollConnection);
        m_autoScrollConnection = QMetaObject::Connection();
    }
}

Qt::KeyboardModifiers TrackItemsListModel::keyboardModifiers() const
{
    Qt::KeyboardModifiers modifiers = QApplication::keyboardModifiers();

    //! NOTE: always treat simultaneously pressed Ctrl and Shift as Ctrl
    if (modifiers.testFlag(Qt::ShiftModifier) && modifiers.testFlag(Qt::ControlModifier)) {
        modifiers = Qt::ControlModifier;
    }

    return modifiers;
}

int TrackItemsListModel::cacheBufferPx()
{
    return CACHE_BUFFER_PX;
}

void TrackItemsListModel::init()
{
    IF_ASSERT_FAILED(m_trackId >= 0) {
        return;
    }

    onSelectedItems(getSelectedItemKeys());

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

    onInit();

    reload();
}

void TrackItemsListModel::reload()
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

    onReload();
}

void TrackItemsListModel::startEditItem(const TrackItemKey& key)
{
    ViewTrackItem* item = itemByKey(key.key);
    if (!item) {
        return;
    }

    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return;
    }

    projectHistory()->startUserInteraction();

    double mousePositionTime = m_context->mousePositionTime();

    vs->setItemEditStartTimeOffset(mousePositionTime - item->time().startTime);
    vs->setItemEditEndTimeOffset(item->time().endTime - mousePositionTime);

    onStartEditItem(key.key);
}

void TrackItemsListModel::endEditItem(const TrackItemKey& key)
{
    ViewTrackItem* item = itemByKey(key.key);
    if (!item) {
        return;
    }

    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return;
    }

    vs->setItemEditStartTimeOffset(-1.0);
    vs->setItemEditEndTimeOffset(-1.0);
    vs->setMoveInitiated(false);

    onEndEditItem(key.key);

    projectHistory()->endUserInteraction();
}

bool TrackItemsListModel::cancelItemDragEdit(const TrackItemKey& key)
{
    ViewTrackItem* item = itemByKey(key.key);
    if (!item) {
        return false;
    }

    auto vs = globalContext()->currentProject()->viewState();
    IF_ASSERT_FAILED(vs) {
        return false;
    }

    vs->setItemEditStartTimeOffset(-1.0);
    vs->setItemEditEndTimeOffset(-1.0);
    vs->setMoveInitiated(false);

    m_context->stopAutoScroll();
    disconnectAutoScroll();

    trackeditInteraction()->cancelItemDragEdit();

    vs->updateClipsBoundaries(true);

    constexpr auto modifyState = false;
    projectHistory()->endUserInteraction(modifyState);

    return true;
}
