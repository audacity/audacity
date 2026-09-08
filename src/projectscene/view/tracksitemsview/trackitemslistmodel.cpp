/*
* Audacity: A Digital Audio Editor
*/
#include "trackitemslistmodel.h"

#include <algorithm>

#include "framework/global/async/async.h"
#include "global/realfn.h"

using namespace au::projectscene;
using namespace au::trackedit;

constexpr int CACHE_BUFFER_PX = 200;

TrackItemsListModel::TrackItemsListModel(QObject* parent)
    : QAbstractListModel(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
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

TrackItemsMoveController* TrackItemsListModel::moveController() const
{
    return m_moveController;
}

void TrackItemsListModel::setMoveController(TrackItemsMoveController* controller)
{
    if (m_moveController == controller) {
        return;
    }
    if (m_moveController) {
        disconnect(m_moveController, nullptr, this, nullptr);
    }
    m_moveController = controller;
    if (m_moveController) {
        connect(m_moveController, &TrackItemsMoveController::previewChanged, this, &TrackItemsListModel::onItemsMoveChanged);
    }
    emit moveControllerChanged();
    onItemsMoveChanged();
}

void TrackItemsListModel::updateItemsMetrics()
{
    for (int i = 0; i < m_items.size(); ++i) {
        updateItemMetrics(m_items[i]);
    }
    updateDragGhostsMetrics();
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

bool TrackItemsListModel::containsItem(const TrackItemKey& key) const
{
    return itemByKey(key.key) != nullptr;
}

double TrackItemsListModel::findGuideline(const TrackItemKey& key, DirectionType::Direction direction) const
{
    ViewTrackItem* item = itemByKey(key.key);
    if (!item) {
        return TimelineContext::INVALID_GUIDELINE_TIME;
    }

    if (direction != DirectionType::Direction::Right) {
        double guidelineTime = m_context->findGuideline(item->time().startTime);
        if (m_context->isGuidelineValid(guidelineTime)) {
            return guidelineTime;
        }
    }
    if (direction != DirectionType::Direction::Left) {
        double guidelineTime = m_context->findGuideline(item->time().endTime);
        if (m_context->isGuidelineValid(guidelineTime)) {
            return guidelineTime;
        }
    }

    return TimelineContext::INVALID_GUIDELINE_TIME;
}

void TrackItemsListModel::setFocusedItem(const TrackItemKey& key)
{
    trackNavigationController()->setFocusedItem(key.key);
}

void TrackItemsListModel::resetFocusedItem()
{
    trackNavigationController()->setFocusedItem({});
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
        emit itemTitleEditRequested(selectedItem->key());
    }
}

int TrackItemsListModel::rowCount(const QModelIndex&) const
{
    return static_cast<int>(m_items.size() + m_dragGhostItems.size());
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
        const int row = index.row();
        ViewTrackItem* item = row < m_items.size() ? m_items.at(row) : m_dragGhostItems.at(row - m_items.size());
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
    if (!ok || completed) {
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
    Qt::KeyboardModifiers modifiers = application()->keyboardModifiers();

    //! NOTE: always treat simultaneously pressed Ctrl and Shift as Ctrl
    if (modifiers.testFlag(Qt::ShiftModifier) && modifiers.testFlag(Qt::ControlModifier)) {
        modifiers = Qt::ControlModifier;
    }

    return modifiers;
}

au::trackedit::SelectionMode TrackItemsListModel::selectionMode() const
{
    const Qt::KeyboardModifiers modifiers = keyboardModifiers();

    if (modifiers.testFlag(Qt::ShiftModifier)) {
        return SelectionMode::Range;
    }

    if (modifiers.testFlag(Qt::ControlModifier)) {
        return SelectionMode::Toggle;
    }

    return SelectionMode::Replace;
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
    selectionController()->clipsIntersectingRangeSelectionChanged().onReceive(this, [this](const ClipKeyList&) {
        updateItemsMetrics();
    });

    trackNavigationController()->focusedItemChanged().onReceive(this, [this](const TrackItemKey& itemKey, bool /*highlight*/) {
        if (itemKey.trackId() != m_trackId) {
            return;
        }

        ViewTrackItem* item = itemByKey(itemKey.key);
        if (item) {
            item->setFocused(true);
        }
    });

    trackNavigationController()->openContextMenuRequested().onReceive(this, [this](const TrackItemKey& key){
        if (key.trackId() != m_trackId) {
            return;
        }

        emit itemContextMenuOpenRequested(key);
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

    prj->trackClipListChanged().onReceive(this, [this](const au::trackedit::Track& track) {
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
    onItemsMoveChanged();
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

    if (vs) {
        vs->setEditedItem(key.key);
        vs->updateItemsBoundaries(true, key.key);
    }

    if (selectionMode() != SelectionMode::Range) {
        setFocusedItem(key);
    }
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
    vs->setEditedItem(trackedit::TrackItemKey {});
    vs->updateItemsBoundaries(false);

    disconnectAutoScroll();

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

    vs->setEditedItem(trackedit::TrackItemKey {});
    vs->updateItemsBoundaries(false);

    constexpr auto modifyState = false;
    projectHistory()->endUserInteraction(modifyState);

    return true;
}

void TrackItemsListModel::onItemsMoveChanged()
{
    for (ViewTrackItem* item : std::as_const(m_items)) {
        item->setDragged(m_moveController && m_moveController->isDragged(item->key().key));
    }

    const trackedit::TrackItemKeyList ghosts = m_moveController
                                               ? m_moveController->itemsOnTrack(m_trackId) : trackedit::TrackItemKeyList {};

    const bool sameItems = std::equal(ghosts.begin(), ghosts.end(), m_dragGhostItems.cbegin(), m_dragGhostItems.cend(),
                                      [](const trackedit::TrackItemKey& key, const ViewTrackItem* item) {
        return key == item->key().key;
    });

    if (!sameItems) {
        const int firstGhostRow = static_cast<int>(m_items.size());
        if (!m_dragGhostItems.isEmpty()) {
            const QList<ViewTrackItem*> oldItems = m_dragGhostItems;
            beginRemoveRows(QModelIndex(), firstGhostRow, firstGhostRow + oldItems.size() - 1);
            m_dragGhostItems.clear();
            endRemoveRows();
            muse::async::Async::call(this, [oldItems]() {
                qDeleteAll(oldItems);
            });
        }
        if (!ghosts.empty()) {
            beginInsertRows(QModelIndex(), firstGhostRow, firstGhostRow + ghosts.size() - 1);
            for (const trackedit::TrackItemKey& key : ghosts) {
                ViewTrackItem* item = createDragGhost(key);
                item->setDragGhost(true);
                item->setSelected(true);
                m_dragGhostItems.append(item);
            }
            endInsertRows();
        }
    }

    updateDragGhostsMetrics();
}

double TrackItemsListModel::moveTimeOffset() const
{
    return m_moveController ? m_moveController->timeOffset() : 0.0;
}

void TrackItemsListModel::updateDragGhostsMetrics()
{
    if (!m_context) {
        return;
    }

    for (ViewTrackItem* item : std::as_const(m_dragGhostItems)) {
        updateItemMetrics(item);
    }
}
