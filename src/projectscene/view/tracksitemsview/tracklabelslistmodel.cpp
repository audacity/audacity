/*
* Audacity: A Digital Audio Editor
*/
#include "tracklabelslistmodel.h"

#include "global/realfn.h"
#include "global/async/async.h"

#include "types/projectscenetypes.h"

#include "log.h"

using namespace au::projectscene;
using namespace au::trackedit;

constexpr int CACHE_BUFFER_PX = 200;

TrackLabelsListModel::TrackLabelsListModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

TrackLabelsListModel::~TrackLabelsListModel()
{
}

void TrackLabelsListModel::init()
{
    IF_ASSERT_FAILED(m_trackId >= 0) {
        return;
    }

    onSelectedLabels(selectionController()->selectedLabels());
    selectionController()->labelsSelected().onReceive(this, [this](const LabelKeyList& keyList) {
        if (keyList.empty()) {
            resetSelectedLabels();
        }

        onSelectedLabels(keyList);
    });

    dispatcher()->reg(this, "rename-label", this, &TrackLabelsListModel::requestLabelTitleChange);

    reload();
}

void TrackLabelsListModel::reload()
{
    if (m_trackId < 0) {
        return;
    }

    ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    prj->trackChanged().onReceive(this, [this](const au::trackedit::Track& track) {
        if (track.id == m_trackId) {
            reload();
        }
    });

    prj->trackRemoved().onReceive(this, [this](const au::trackedit::Track& track) {
        if (track.id == m_trackId) {
            m_trackId = -1;
        }
    });

    m_allLabelList = prj->labelList(m_trackId);

    //! NOTE Labels in the track may not be in order (relative to startTime), here we arrange them.
    std::sort(m_allLabelList.begin(), m_allLabelList.end(), [](const Label& l1, const Label& l2) {
        return l1.startTime < l2.startTime;
    });

    //! NOTE Reload everything if the list has changed completely
    m_allLabelList.onChanged(this, [this]() {
        muse::async::Async::call(this, [this]() {
            reload();
        });
    });

    m_allLabelList.onItemChanged(this, [this](const Label& label) {
        for (size_t i = 0; i < m_allLabelList.size(); ++i) {
            if (m_allLabelList.at(i).key != label.key) {
                continue;
            }
            m_allLabelList[i] = label;
            break;
        }

        LabelListItem* item = itemByKey(label.key);
        if (item) {
            item->setLabel(label);
        }

        updateItemsMetrics();
    });

    m_allLabelList.onItemAdded(this, [this](const Label& label) {
        ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        muse::async::NotifyList<au::trackedit::Label> newList = prj->labelList(m_trackId);
        for (size_t i = 0; i < newList.size(); ++i) {
            if (newList.at(i).key != label.key) {
                continue;
            }

            m_allLabelList.insert(m_allLabelList.begin() + i, label);

            update();

            emit m_labelList.at(i)->titleEditRequested();

            break;
        }
    });

    m_allLabelList.onItemRemoved(this, [this](const Label& label) {
        for (auto it = m_allLabelList.begin(); it != m_allLabelList.end(); ++it) {
            if (it->key == label.key) {
                m_allLabelList.erase(it);
                update();
                break;
            }
        }
    });

    update();
}

void TrackLabelsListModel::startEditLabel(const LabelKey& key)
{
    LabelListItem* item = itemByKey(key.key);
    if (!item) {
        return;
    }

    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return;
    }

    double mousePositionTime = m_context->mousePositionTime();

    vs->setObjectEditStartTimeOffset(mousePositionTime - item->label().startTime);
    vs->setObjectEditEndTimeOffset(item->label().endTime - mousePositionTime);
}

void TrackLabelsListModel::endEditLabel(const LabelKey& key)
{
    LabelListItem* item = itemByKey(key.key);
    if (!item) {
        return;
    }

    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return;
    }

    // disconnectAutoScroll();

    vs->setObjectEditStartTimeOffset(-1.0);
    vs->setObjectEditEndTimeOffset(-1.0);
    vs->setMoveInitiated(false);
}

LabelListItem* TrackLabelsListModel::itemByKey(const trackedit::LabelKey& k) const
{
    for (LabelListItem* item : std::as_const(m_labelList)) {
        if (item->label().key != k) {
            continue;
        }
        return item;
    }
    return nullptr;
}

int TrackLabelsListModel::indexByKey(const trackedit::LabelKey& k) const
{
    for (int i = 0; i < m_labelList.size(); ++i) {
        if (m_labelList.at(i)->label().key == k) {
            return i;
        }
    }
    return -1;
}

Qt::KeyboardModifiers TrackLabelsListModel::keyboardModifiers() const
{
    Qt::KeyboardModifiers modifiers = QApplication::keyboardModifiers();

    //! NOTE: always treat simultaneously pressed Ctrl and Shift as Ctrl
    if (modifiers.testFlag(Qt::ShiftModifier) && modifiers.testFlag(Qt::ControlModifier)) {
        modifiers = Qt::ControlModifier;
    }

    return modifiers;
}

void TrackLabelsListModel::update()
{
    std::unordered_map<LabelId, LabelListItem*> oldItems;
    for (int row = 0; row < m_labelList.size(); ++row) {
        oldItems.emplace(m_labelList[row]->key().key.objectId, m_labelList[row]);
    }

    QList<LabelListItem*> newList;

    // Building a new list, reusing existing labels
    for (const au::trackedit::Label& l : m_allLabelList) {
        auto it = oldItems.find(l.key.objectId);
        LabelListItem* item = nullptr;

        if (it != oldItems.end()) {
            item = it->second;
            oldItems.erase(it);
        } else {
            item = new LabelListItem(this);
        }

        item->setLabel(l);
        newList.append(item);
    }

    // Removing deleted or moved labels
    // Item deletion should be postponed, so QML is updated correctly
    QList<LabelListItem*> cleanupList;
    for (auto& [id, item] : oldItems) {
        int row = m_labelList.indexOf(item);
        if (row >= 0) {
            beginRemoveRows(QModelIndex(), row, row);
            m_labelList.removeAt(row);
            endRemoveRows();
        }
        cleanupList.append(item);
    }

    // Sorting labels with a notification for each moved label
    for (int i = 0; i < newList.size(); ++i) {
        LabelListItem* item = newList[i];
        if (i < m_labelList.size() && m_labelList[i] == item) {
            // TODO: is it possible to know if update is neccessary?
            QModelIndex idx = index(i);
            emit dataChanged(idx, idx);
        } else {
            // If the label was already present, then moving
            int oldIndex = m_labelList.indexOf(item);
            if (oldIndex >= 0) {
                beginMoveRows(QModelIndex(), oldIndex, oldIndex, QModelIndex(), i > oldIndex ? i + 1 : i);
                m_labelList.move(oldIndex, i);
                endMoveRows();
            } else {
                beginInsertRows(QModelIndex(), i, i);
                m_labelList.insert(i, item);
                endInsertRows();
            }
        }
    }

    updateItemsMetrics();

    //! NOTE We need to update the selected items
    //! to take pointers to the items from the new list
    m_selectedItems.clear();
    onSelectedLabels(selectionController()->selectedLabels());

    muse::async::Async::call(this, [cleanupList]() {
        qDeleteAll(cleanupList);
    });
}

void TrackLabelsListModel::updateItemsMetrics()
{
    for (int i = 0; i < m_labelList.size(); ++i) {
        LabelListItem* item = m_labelList[i];
        updateItemsMetrics(item);
    }
}

void TrackLabelsListModel::updateItemsMetrics(LabelListItem* item)
{
    //! NOTE The first step is to calculate the position and width
    const double cacheTime = CACHE_BUFFER_PX / m_context->zoom();

    const trackedit::Label& label = item->label();

    LabelTime time;
    time.startTime = label.startTime;
    time.endTime = label.endTime;
    time.itemStartTime = std::max(label.startTime, (m_context->frameStartTime() - cacheTime));
    time.itemEndTime = std::min(label.endTime, (m_context->frameEndTime() + cacheTime));

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

int TrackLabelsListModel::rowCount(const QModelIndex&) const
{
    return static_cast<int>(m_labelList.size());
}

QHash<int, QByteArray> TrackLabelsListModel::roleNames() const
{
    static QHash<int, QByteArray> roles
    {
        { LabelItemRole, "item" }
    };
    return roles;
}

QVariant TrackLabelsListModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid()) {
        return QVariant();
    }

    switch (role) {
    case LabelItemRole: {
        LabelListItem* item = m_labelList.at(index.row());
        return QVariant::fromValue(item);
    }
    default:
        break;
    }

    return QVariant();
}

void TrackLabelsListModel::onTimelineZoomChanged()
{
    updateItemsMetrics();
}

void TrackLabelsListModel::onTimelineFrameTimeChanged()
{
    updateItemsMetrics();
}

void TrackLabelsListModel::setSelectedItems(const QList<LabelListItem*>& items)
{
    for (auto& selectedItem : m_selectedItems) {
        selectedItem->setSelected(false);
    }
    m_selectedItems = items;
    for (auto& selectedItem : m_selectedItems) {
        selectedItem->setSelected(true);
    }
}

void TrackLabelsListModel::addSelectedItem(LabelListItem* item)
{
    item->setSelected(true);
    m_selectedItems.append(item);
}

void TrackLabelsListModel::clearSelectedItems()
{
    for (auto& selectedItem : m_selectedItems) {
        selectedItem->setSelected(false);
    }
    m_selectedItems.clear();
}

bool TrackLabelsListModel::changeLabelTitle(const LabelKey& key, const QString& newTitle)
{
    bool ok = trackeditInteraction()->changeLabelTitle(key.key, muse::String::fromQString(newTitle));
    return ok;
}

QVariant TrackLabelsListModel::next(const LabelKey& key) const
{
    return neighbor(key, 1);
}

QVariant TrackLabelsListModel::prev(const LabelKey& key) const
{
    return neighbor(key, -1);
}

QVariant TrackLabelsListModel::neighbor(const LabelKey& key, int offset) const
{
    auto it = std::find_if(m_labelList.begin(), m_labelList.end(), [key](LabelListItem* label) {
        return label->key().key.objectId == key.key.objectId;
    });

    if (it == m_labelList.end()) {
        return QVariant();
    }

    int sortedIndex = std::distance(m_labelList.begin(), it) + offset;
    if (sortedIndex < 0 || sortedIndex >= m_labelList.size()) {
        return QVariant();
    }

    return QVariant::fromValue(m_labelList[sortedIndex]);
}

void TrackLabelsListModel::selectLabel(const LabelKey& key)
{
    Qt::KeyboardModifiers modifiers = keyboardModifiers();

    if (modifiers.testFlag(Qt::ShiftModifier)) {
        selectionController()->addSelectedLabel(key.key);
    } else {
        if (muse::contains(selectionController()->selectedLabels(), key.key)) {
            return;
        }
        selectionController()->setSelectedLabels(LabelKeyList({ key.key }), true);
    }
}

void TrackLabelsListModel::resetSelectedLabels()
{
    clearSelectedItems();
    selectionController()->resetSelectedLabels();
}

void TrackLabelsListModel::requestLabelTitleChange()
{
    auto selectedLabels = selectionController()->selectedLabels();

    if (selectedLabels.empty() || selectedLabels.size() > 1) {
        return;
    }

    trackedit::LabelKey labelKey = selectedLabels.front();
    if (!labelKey.isValid()) {
        return;
    }

    LabelListItem* selectedItem = itemByKey(labelKey);
    if (selectedItem != nullptr) {
        emit selectedItem->titleEditRequested();
    }
}

void TrackLabelsListModel::onSelectedLabel(const trackedit::LabelKey& k)
{
    // ignore if item already selected
    for (const auto& selectedItem : m_selectedItems) {
        if (selectedItem->label().key == k) {
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
                setSelectedItems(QList<LabelListItem*>({ item }));
            }
        }
    }
}

void TrackLabelsListModel::onSelectedLabels(const trackedit::LabelKeyList& keyList)
{
    if (keyList.size() == 1) {
        onSelectedLabel(keyList.front());
        return;
    }

    // Multiple-label selection can only be done programmatically, hence there is no need to check for the Shift key ;
    // we can begin by clearing everything.
    clearSelectedItems();

    QList<LabelListItem*> items;
    for (const auto& k : keyList) {
        if (const auto item = itemByKey(k)) {
            items.append(item);
        }
    }
    setSelectedItems(items);
}

QVariant TrackLabelsListModel::trackId() const
{
    return QVariant::fromValue(m_trackId);
}

void TrackLabelsListModel::setTrackId(const QVariant& _newTrackId)
{
    trackedit::TrackId newTrackId = _newTrackId.toInt();
    if (m_trackId == newTrackId) {
        return;
    }
    m_trackId = newTrackId;
    emit trackIdChanged();
}

TimelineContext* TrackLabelsListModel::timelineContext() const
{
    return m_context;
}

void TrackLabelsListModel::setTimelineContext(TimelineContext* newContext)
{
    if (m_context == newContext) {
        return;
    }

    if (m_context) {
        disconnect(m_context, nullptr, this, nullptr);
    }

    m_context = newContext;

    if (m_context) {
        connect(m_context, &TimelineContext::zoomChanged, this, &TrackLabelsListModel::onTimelineZoomChanged);
        connect(m_context, &TimelineContext::frameTimeChanged, this, &TrackLabelsListModel::onTimelineFrameTimeChanged);
    }

    emit timelineContextChanged();
}

int TrackLabelsListModel::cacheBufferPx()
{
    return CACHE_BUFFER_PX;
}
