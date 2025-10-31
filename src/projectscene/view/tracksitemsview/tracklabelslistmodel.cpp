/*
* Audacity: A Digital Audio Editor
*/
#include "tracklabelslistmodel.h"

#include "global/async/async.h"

#include "types/projectscenetypes.h"

#include "log.h"

using namespace au::projectscene;
using namespace au::trackedit;

TrackLabelsListModel::TrackLabelsListModel(QObject* parent)
    : TrackItemsListModel(parent)
{
}

void TrackLabelsListModel::onInit()
{
    selectionController()->labelsSelected().onReceive(this, [this](const LabelKeyList& keyList) {
        if (keyList.empty()) {
            resetSelectedLabels();
        }

        onSelectedItems(keyList);
    });

    dispatcher()->reg(this, "rename-label", [this]() {
        requestItemTitleChange();
    });
}

void TrackLabelsListModel::onReload()
{
    ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    IF_ASSERT_FAILED(prj) {
        return;
    }

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

        TrackLabelItem* item = labelItemByKey(label.key);
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

            emit static_cast<TrackLabelItem*>(m_items.at(i))->titleEditRequested();

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

TrackLabelItem* TrackLabelsListModel::labelItemByKey(const trackedit::LabelKey& k) const
{
    return static_cast<TrackLabelItem*>(itemByKey(k));
}

void TrackLabelsListModel::update()
{
    std::unordered_map<LabelId, TrackLabelItem*> oldItems;
    for (int row = 0; row < m_items.size(); ++row) {
        TrackLabelItem* labelItem = static_cast<TrackLabelItem*>(m_items[row]);
        oldItems.emplace(labelItem->key().key.itemId, labelItem);
    }

    QList<TrackLabelItem*> newList;

    // Building a new list, reusing existing labels
    for (const au::trackedit::Label& l : m_allLabelList) {
        auto it = oldItems.find(l.key.itemId);
        TrackLabelItem* item = nullptr;

        if (it != oldItems.end()) {
            item = it->second;
            oldItems.erase(it);
        } else {
            item = new TrackLabelItem(this);
        }

        item->setLabel(l);
        newList.append(item);
    }

    // Removing deleted or moved labels
    // Item deletion should be postponed, so QML is updated correctly
    QList<TrackLabelItem*> cleanupList;
    for (auto& [id, item] : oldItems) {
        int row = m_items.indexOf(item);
        if (row >= 0) {
            beginRemoveRows(QModelIndex(), row, row);
            m_items.removeAt(row);
            endRemoveRows();
        }
        cleanupList.append(item);
    }

    // Sorting labels with a notification for each moved label
    for (int i = 0; i < newList.size(); ++i) {
        TrackLabelItem* item = newList[i];
        if (i < m_items.size() && m_items[i] == item) {
            // TODO: is it possible to know if update is neccessary?
            QModelIndex idx = index(i);
            emit dataChanged(idx, idx);
        } else {
            // If the label was already present, then moving
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
    onSelectedItems(selectionController()->selectedLabels());

    muse::async::Async::call(this, [cleanupList]() {
        qDeleteAll(cleanupList);
    });
}

void TrackLabelsListModel::updateItemMetrics(ViewTrackItem* viewItem)
{
    TrackLabelItem* item = static_cast<TrackLabelItem*>(viewItem);

    ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    trackedit::Label label = prj->label(item->key().key);
    if (!label.isValid()) {
        return;
    }

    //! NOTE The first step is to calculate the position and width
    const double cacheTime = cacheBufferPx() / m_context->zoom();

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

bool TrackLabelsListModel::changeLabelTitle(const LabelKey& key, const QString& newTitle)
{
    return trackeditInteraction()->changeLabelTitle(key.key, muse::String::fromQString(newTitle));
}

bool TrackLabelsListModel::moveSelectedLabels(const LabelKey& key, bool completed)
{
    TrackLabelItem* item = labelItemByKey(key.key);
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

    TrackItemsListModel::MoveOffset moveOffset = calculateMoveOffset(item, key, completed);
    if (vs->moveInitiated()) {
        trackeditInteraction()->moveLabels(moveOffset.timeOffset, completed);
    }

    if ((completed && m_autoScrollConnection)) {
        disconnectAutoScroll();
    } else if (!m_autoScrollConnection && !completed) {
        m_autoScrollConnection = connect(m_context, &TimelineContext::frameTimeChanged, [this, key](){ moveSelectedLabels(key, false); });
    }

    return true;
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

TrackItemKeyList TrackLabelsListModel::getSelectedItemKeys() const
{
    return selectionController()->selectedLabels();
}

bool TrackLabelsListModel::stretchLabelLeft(const LabelKey& key, bool completed)
{
    auto project = globalContext()->currentProject();
    IF_ASSERT_FAILED(project) {
        return false;
    }

    auto vs = project->viewState();
    IF_ASSERT_FAILED(vs) {
        return false;
    }

    double newStartTime = m_context->mousePositionTime() - vs->itemEditStartTimeOffset();
    if (vs->isSnapEnabled()) {
        newStartTime = m_context->applySnapToTime(newStartTime);
    } else {
        newStartTime = m_context->applySnapToItem(newStartTime);
    }

    bool ok = trackeditInteraction()->stretchLabelLeft(key.key, newStartTime, completed);

    handleAutoScroll(ok, completed, [this, key]() {
        stretchLabelLeft(key, false);
    });

    return ok;
}

bool TrackLabelsListModel::stretchLabelRight(const LabelKey& key, bool completed)
{
    auto project = globalContext()->currentProject();
    IF_ASSERT_FAILED(project) {
        return false;
    }

    auto vs = project->viewState();
    IF_ASSERT_FAILED(vs) {
        return false;
    }

    double newEndTime = m_context->mousePositionTime() + vs->itemEditEndTimeOffset();
    if (vs->isSnapEnabled()) {
        newEndTime = m_context->applySnapToTime(newEndTime);
    } else {
        newEndTime = m_context->applySnapToItem(newEndTime);
    }

    bool ok = trackeditInteraction()->stretchLabelRight(key.key, newEndTime, completed);

    handleAutoScroll(ok, completed, [this, key]() {
        stretchLabelRight(key, false);
    });

    return ok;
}
