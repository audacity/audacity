/****************************************************************************
**
** Copyright (C) 2016 The Qt Company Ltd.
** Contact: https://www.qt.io/licensing/
**
** This file is part of the Qt Quick Controls module of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:LGPL$
** Commercial License Usage
** Licensees holding valid commercial Qt licenses may use this file in
** accordance with the commercial license agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and The Qt Company. For licensing terms
** and conditions see https://www.qt.io/terms-conditions. For further
** information use the contact form at https://www.qt.io/contact-us.
**
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 3 as published by the Free Software
** Foundation and appearing in the file LICENSE.LGPL3 included in the
** packaging of this file. Please review the following information to
** ensure the GNU Lesser General Public License version 3 requirements
** will be met: https://www.gnu.org/licenses/lgpl-3.0.html.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU
** General Public License version 2.0 or (at your option) the GNU General
** Public license version 3 or any later version approved by the KDE Free
** Qt Foundation. The licenses are as published by the Free Software
** Foundation and appearing in the file LICENSE.GPL2 and LICENSE.GPL3
** included in the packaging of this file. Please review the following
** information to ensure the GNU General Public License requirements will
** be met: https://www.gnu.org/licenses/gpl-2.0.html and
** https://www.gnu.org/licenses/gpl-3.0.html.
**
** $QT_END_LICENSE$
**
****************************************************************************/

#include <math.h>
#include "qquicktreemodeladaptor_p.h"
#include <QtCore/qstack.h>
#include <QtCore/qdebug.h>

QT_BEGIN_NAMESPACE

//#define QQUICKTREEMODELADAPTOR_DEBUG
#if defined(QQUICKTREEMODELADAPTOR_DEBUG) && !defined(QT_TESTLIB_LIB)
#   define ASSERT_CONSISTENCY() Q_ASSERT_X(testConsistency(true /* dumpOnFail */), Q_FUNC_INFO, "Consistency test failed")
#else
#   define ASSERT_CONSISTENCY qt_noop
#endif

QQuickTreeModelAdaptor1::QQuickTreeModelAdaptor1(QObject *parent) :
    QAbstractListModel(parent), m_model(0), m_lastItemIndex(0),
    m_visibleRowsMoved(false), m_signalAggregatorStack(0)
{
}

QAbstractItemModel *QQuickTreeModelAdaptor1::model() const
{
    return m_model;
}

void QQuickTreeModelAdaptor1::setModel(QAbstractItemModel *arg)
{
    struct Cx {
        const char *signal;
        const char *slot;
    };
    static const Cx connections[] = {
        { SIGNAL(destroyed(QObject*)),
          SLOT(modelHasBeenDestroyed()) },
        { SIGNAL(modelReset()),
          SLOT(modelHasBeenReset()) },
        { SIGNAL(dataChanged(const QModelIndex&, const QModelIndex&, const QVector<int>&)),
          SLOT(modelDataChanged(const QModelIndex&, const QModelIndex&, const QVector<int>&)) },

        { SIGNAL(layoutAboutToBeChanged(const QList<QPersistentModelIndex>&, QAbstractItemModel::LayoutChangeHint)),
          SLOT(modelLayoutAboutToBeChanged(const QList<QPersistentModelIndex>&, QAbstractItemModel::LayoutChangeHint)) },
        { SIGNAL(layoutChanged(const QList<QPersistentModelIndex>&, QAbstractItemModel::LayoutChangeHint)),
          SLOT(modelLayoutChanged(const QList<QPersistentModelIndex>&, QAbstractItemModel::LayoutChangeHint)) },

        { SIGNAL(rowsAboutToBeInserted(const QModelIndex&, int, int)),
          SLOT(modelRowsAboutToBeInserted(const QModelIndex &, int, int)) },
        { SIGNAL(rowsInserted(const QModelIndex&, int, int)),
          SLOT(modelRowsInserted(const QModelIndex&, int, int)) },
        { SIGNAL(rowsAboutToBeRemoved(const QModelIndex&, int, int)),
          SLOT(modelRowsAboutToBeRemoved(const QModelIndex&, int, int)) },
        { SIGNAL(rowsRemoved(const QModelIndex&, int, int)),
          SLOT(modelRowsRemoved(const QModelIndex&, int, int)) },
        { SIGNAL(rowsAboutToBeMoved(const QModelIndex&, int, int, const QModelIndex&, int)),
          SLOT(modelRowsAboutToBeMoved(const QModelIndex&, int, int, const QModelIndex&, int)) },
        { SIGNAL(rowsMoved(const QModelIndex&, int, int, const QModelIndex&, int)),
          SLOT(modelRowsMoved(const QModelIndex&, int, int, const QModelIndex&, int)) },
        { 0, 0 }
    };

    if (m_model != arg) {
        if (m_model) {
            for (const Cx *c = &connections[0]; c->signal; c++)
                disconnect(m_model, c->signal, this, c->slot);
        }

        clearModelData();
        m_model = arg;

        if (m_model) {
            for (const Cx *c = &connections[0]; c->signal; c++)
                connect(m_model, c->signal, this, c->slot);

            showModelTopLevelItems();
        }

        emit modelChanged(arg);
    }
}

void QQuickTreeModelAdaptor1::clearModelData()
{
    beginResetModel();
    m_items.clear();
    m_expandedItems.clear();
    endResetModel();
}

const QModelIndex QQuickTreeModelAdaptor1::rootIndex() const
{
    return m_rootIndex;
}

void QQuickTreeModelAdaptor1::setRootIndex(const QModelIndex &idx)
{
    if (m_rootIndex == idx)
        return;

    if (m_model)
        clearModelData();
    m_rootIndex = idx;
    if (m_model)
        showModelTopLevelItems();
    emit rootIndexChanged();
}

void QQuickTreeModelAdaptor1::resetRootIndex()
{
    setRootIndex(QModelIndex());
}

QHash<int, QByteArray> QQuickTreeModelAdaptor1::roleNames() const
{
    if (!m_model)
        return QHash<int, QByteArray>();

    QHash<int, QByteArray> modelRoleNames = m_model->roleNames();
    modelRoleNames.insert(DepthRole, "_q_TreeView_ItemDepth");
    modelRoleNames.insert(ExpandedRole, "_q_TreeView_ItemExpanded");
    modelRoleNames.insert(HasChildrenRole, "_q_TreeView_HasChildren");
    modelRoleNames.insert(HasSiblingRole, "_q_TreeView_HasSibling");
    modelRoleNames.insert(ModelIndexRole, "_q_TreeView_ModelIndex");
    return modelRoleNames;
}

int QQuickTreeModelAdaptor1::rowCount(const QModelIndex &) const
{
    return m_items.count();
}

QVariant QQuickTreeModelAdaptor1::data(const QModelIndex &index, int role) const
{
    if (!m_model)
        return QVariant();

    const QModelIndex &modelIndex = mapToModel(index);

    switch (role) {
    case DepthRole:
        return m_items.at(index.row()).depth;
    case ExpandedRole:
        return isExpanded(index.row());
    case HasChildrenRole:
        return !(modelIndex.flags() & Qt::ItemNeverHasChildren) && m_model->hasChildren(modelIndex);
    case HasSiblingRole:
        return modelIndex.row() != m_model->rowCount(modelIndex.parent()) - 1;
    case ModelIndexRole:
        return modelIndex;
    default:
        return m_model->data(modelIndex, role);
    }
}

bool QQuickTreeModelAdaptor1::setData(const QModelIndex &index, const QVariant &value, int role)
{
    if (!m_model)
        return false;

    switch (role) {
    case DepthRole:
    case ExpandedRole:
    case HasChildrenRole:
    case HasSiblingRole:
    case ModelIndexRole:
        return false;
    default: {
        const QModelIndex &pmi = mapToModel(index);
        return m_model->setData(pmi, value, role);
    }
    }
}

int QQuickTreeModelAdaptor1::itemIndex(const QModelIndex &index) const
{
    // This is basically a plagiarism of QTreeViewPrivate::viewIndex()
    if (!index.isValid() || index == m_rootIndex || m_items.isEmpty())
        return -1;

    const int totalCount = m_items.count();

    // We start nearest to the lastViewedItem
    int localCount = qMin(m_lastItemIndex - 1, totalCount - m_lastItemIndex);
    for (int i = 0; i < localCount; ++i) {
        const TreeItem &item1 = m_items.at(m_lastItemIndex + i);
        if (item1.index == index) {
            m_lastItemIndex = m_lastItemIndex + i;
            return m_lastItemIndex;
        }
        const TreeItem &item2 = m_items.at(m_lastItemIndex - i - 1);
        if (item2.index == index) {
            m_lastItemIndex = m_lastItemIndex - i - 1;
            return m_lastItemIndex;
        }
    }

    for (int j = qMax(0, m_lastItemIndex + localCount); j < totalCount; ++j) {
        const TreeItem &item = m_items.at(j);
        if (item.index == index) {
            m_lastItemIndex = j;
            return j;
        }
    }
    for (int j = qMin(totalCount, m_lastItemIndex - localCount) - 1; j >= 0; --j) {
        const TreeItem &item = m_items.at(j);
        if (item.index == index) {
            m_lastItemIndex = j;
            return j;
        }
    }

    // nothing found
    return -1;
}

bool QQuickTreeModelAdaptor1::isVisible(const QModelIndex &index)
{
    return itemIndex(index) != -1;
}

bool QQuickTreeModelAdaptor1::childrenVisible(const QModelIndex &index)
{
    return (index == m_rootIndex && !m_items.isEmpty())
           || (m_expandedItems.contains(index) && isVisible(index));
}

QModelIndex QQuickTreeModelAdaptor1::mapToModel(const QModelIndex &index) const
{
    return m_items.at(index.row()).index;
}

QPersistentModelIndex QQuickTreeModelAdaptor1::mapRowToModelIndex(int row) const
{
    if (!m_model)
        return QModelIndex();
    if (row < 0 || row >= m_items.count())
        return QModelIndex();
    return m_items.at(row).index;
}

QItemSelection  QQuickTreeModelAdaptor1::selectionForRowRange(const QModelIndex &fromIndex, const QModelIndex &toIndex) const
{
    int from = itemIndex(fromIndex);
    int to = itemIndex(toIndex);
    if (from == -1) {
        if (to == -1)
            return QItemSelection();
        return QItemSelection(toIndex, toIndex);
    }

    to = qMax(to, 0);
    if (from > to)
        qSwap(from, to);

    typedef QPair<QModelIndex, QModelIndex> MIPair;
    typedef QHash<QModelIndex, MIPair> MI2MIPairHash;
    MI2MIPairHash ranges;
    QModelIndex firstIndex = m_items.at(from).index;
    QModelIndex lastIndex = firstIndex;
    QModelIndex previousParent = firstIndex.parent();
    bool selectLastRow = false;
    for (int i = from + 1; i <= to || (selectLastRow = true); i++) {
        // We run an extra iteration to make sure the last row is
        // added to the selection. (And also to avoid duplicating
        // the insertion code.)
        QModelIndex index;
        QModelIndex parent;
        if (!selectLastRow) {
            index = m_items.at(i).index;
            parent = index.parent();
        }
        if (selectLastRow || previousParent != parent) {
            const MI2MIPairHash::iterator &it = ranges.find(previousParent);
            if (it == ranges.end())
                ranges.insert(previousParent, MIPair(firstIndex, lastIndex));
            else
                it->second = lastIndex;

            if (selectLastRow)
                break;

            firstIndex = index;
            previousParent = parent;
        }
        lastIndex = index;
    }

    QItemSelection sel;
    sel.reserve(ranges.count());
    for (const MIPair &pair : qAsConst(ranges))
       sel.append(QItemSelectionRange(pair.first, pair.second));

    return sel;
}

void QQuickTreeModelAdaptor1::showModelTopLevelItems(bool doInsertRows)
{
    if (!m_model)
        return;

    if (m_model->hasChildren(m_rootIndex) && m_model->canFetchMore(m_rootIndex))
        m_model->fetchMore(m_rootIndex);
    const long topLevelRowCount = m_model->rowCount(m_rootIndex);
    if (topLevelRowCount == 0)
        return;

    showModelChildItems(TreeItem(m_rootIndex), 0, topLevelRowCount - 1, doInsertRows);
}

void QQuickTreeModelAdaptor1::showModelChildItems(const TreeItem &parentItem, int start, int end, bool doInsertRows, bool doExpandPendingRows)
{
    const QModelIndex &parentIndex = parentItem.index;
    int rowIdx = parentIndex.isValid() && parentIndex != m_rootIndex ? itemIndex(parentIndex) + 1 : 0;
    Q_ASSERT(rowIdx == 0 || parentItem.expanded);
    if (parentIndex.isValid() && parentIndex != m_rootIndex && (rowIdx == 0 || !parentItem.expanded))
        return;

    if (m_model->rowCount(parentIndex) == 0) {
        if (m_model->hasChildren(parentIndex) && m_model->canFetchMore(parentIndex))
            m_model->fetchMore(parentIndex);
        return;
    }

    int insertCount = end - start + 1;
    int startIdx;
    if (start == 0) {
        startIdx = rowIdx;
    } else {
        // Prefer to insert before next sibling instead of after last child of previous, as
        // the latter is potentially buggy, see QTBUG-66062
        const QModelIndex &nextSiblingIdx = m_model->index(end + 1, 0, parentIndex);
        if (nextSiblingIdx.isValid()) {
            startIdx = itemIndex(nextSiblingIdx);
        } else {
            const QModelIndex &prevSiblingIdx = m_model->index(start - 1, 0, parentIndex);
            startIdx = lastChildIndex(prevSiblingIdx) + 1;
        }
    }

    int rowDepth = rowIdx == 0 ? 0 : parentItem.depth + 1;
    if (doInsertRows)
        beginInsertRows(QModelIndex(), startIdx, startIdx + insertCount - 1);
    m_items.reserve(m_items.count() + insertCount);
    for (int i = 0; i < insertCount; i++) {
        const QModelIndex &cmi = m_model->index(start + i, 0, parentIndex);
        bool expanded = m_expandedItems.contains(cmi);
        m_items.insert(startIdx + i, TreeItem(cmi, rowDepth, expanded));
        if (expanded)
            m_itemsToExpand.append(&m_items[startIdx + i]);
    }
    if (doInsertRows)
        endInsertRows();

    if (doExpandPendingRows)
        expandPendingRows(doInsertRows);
}


void QQuickTreeModelAdaptor1::expand(const QModelIndex &idx)
{
    ASSERT_CONSISTENCY();
    if (!m_model)
        return;
    Q_ASSERT(!idx.isValid() || idx.model() == m_model);
    if (!idx.isValid() || !m_model->hasChildren(idx))
        return;
    if (m_expandedItems.contains(idx))
        return;

    int row = itemIndex(idx);
    if (row != -1)
        expandRow(row);
    else
        m_expandedItems.insert(idx);
    ASSERT_CONSISTENCY();

    emit expanded(idx);
}

void QQuickTreeModelAdaptor1::collapse(const QModelIndex &idx)
{
    ASSERT_CONSISTENCY();
    if (!m_model)
        return;
    Q_ASSERT(!idx.isValid() || idx.model() == m_model);
    if (!idx.isValid() || !m_model->hasChildren(idx))
        return;
    if (!m_expandedItems.contains(idx))
        return;

    int row = itemIndex(idx);
    if (row != -1)
        collapseRow(row);
    else
        m_expandedItems.remove(idx);
    ASSERT_CONSISTENCY();

    emit collapsed(idx);
}

bool QQuickTreeModelAdaptor1::isExpanded(const QModelIndex &index) const
{
    ASSERT_CONSISTENCY();
    if (!m_model)
        return false;
    Q_ASSERT(!index.isValid() || index.model() == m_model);
    return !index.isValid() || m_expandedItems.contains(index);
}

bool QQuickTreeModelAdaptor1::isExpanded(int row) const
{
    return m_items.at(row).expanded;
}

void QQuickTreeModelAdaptor1::expandRow(int n)
{
    if (!m_model || isExpanded(n))
        return;

    TreeItem &item = m_items[n];
    if ((item.index.flags() & Qt::ItemNeverHasChildren) || !m_model->hasChildren(item.index))
        return;
    item.expanded = true;
    m_expandedItems.insert(item.index);
    QVector<int> changedRole(1, ExpandedRole);
    emit dataChanged(index(n), index(n), changedRole);

    m_itemsToExpand.append(&item);
    expandPendingRows();
}

void QQuickTreeModelAdaptor1::expandPendingRows(bool doInsertRows)
{
    while (!m_itemsToExpand.isEmpty()) {
        TreeItem *item = m_itemsToExpand.takeFirst();
        Q_ASSERT(item->expanded);
        const QModelIndex &index = item->index;
        int childrenCount = m_model->rowCount(index);
        if (childrenCount == 0) {
            if (m_model->hasChildren(index) && m_model->canFetchMore(index))
                m_model->fetchMore(index);
            continue;
        }

        // TODO Pre-compute the total number of items made visible
        // so that we only call a single beginInsertRows()/endInsertRows()
        // pair per expansion (same as we do for collapsing).
        showModelChildItems(*item, 0, childrenCount - 1, doInsertRows, false);
    }
}

void QQuickTreeModelAdaptor1::collapseRow(int n)
{
    if (!m_model || !isExpanded(n))
        return;

    SignalFreezer aggregator(this);

    TreeItem &item = m_items[n];
    item.expanded = false;
    m_expandedItems.remove(item.index);
    QVector<int> changedRole(1, ExpandedRole);
    queueDataChanged(index(n), index(n), changedRole);
    int childrenCount = m_model->rowCount(item.index);
    if ((item.index.flags() & Qt::ItemNeverHasChildren) || !m_model->hasChildren(item.index) || childrenCount == 0)
        return;

    const QModelIndex &emi = m_model->index(childrenCount - 1, 0, item.index);
    int lastIndex = lastChildIndex(emi);
    removeVisibleRows(n + 1, lastIndex);
}

int QQuickTreeModelAdaptor1::lastChildIndex(const QModelIndex &index)
{
    if (!m_expandedItems.contains(index))
        return itemIndex(index);

    QModelIndex parent = index.parent();
    QModelIndex nextSiblingIndex;
    while (parent.isValid()) {
        nextSiblingIndex = parent.sibling(parent.row() + 1, 0);
        if (nextSiblingIndex.isValid())
            break;
        parent = parent.parent();
    }

    int firstIndex = nextSiblingIndex.isValid() ? itemIndex(nextSiblingIndex) : m_items.count();
    return firstIndex - 1;
}

void QQuickTreeModelAdaptor1::removeVisibleRows(int startIndex, int endIndex, bool doRemoveRows)
{
    if (startIndex < 0 || endIndex < 0 || startIndex > endIndex)
        return;

    if (doRemoveRows)
        beginRemoveRows(QModelIndex(), startIndex, endIndex);
    m_items.erase(m_items.begin() + startIndex, m_items.begin() + endIndex + 1);
    if (doRemoveRows) {
        endRemoveRows();

        /* We need to update the model index for all the items below the removed ones */
        int lastIndex = m_items.count() - 1;
        if (startIndex <= lastIndex) {
            const QModelIndex &topLeft = index(startIndex, 0, QModelIndex());
            const QModelIndex &bottomRight = index(lastIndex, 0, QModelIndex());
            const QVector<int> changedRole(1, ModelIndexRole);
            queueDataChanged(topLeft, bottomRight, changedRole);
        }
    }
}

void QQuickTreeModelAdaptor1::modelHasBeenDestroyed()
{
    // The model has been deleted. This should behave as if no model was set
    clearModelData();
    emit modelChanged(nullptr);
}

void QQuickTreeModelAdaptor1::modelHasBeenReset()
{
    clearModelData();

    showModelTopLevelItems();
    ASSERT_CONSISTENCY();
}

void QQuickTreeModelAdaptor1::modelDataChanged(const QModelIndex &topLeft, const QModelIndex &bottomRigth, const QVector<int> &roles)
{
    Q_ASSERT(topLeft.parent() == bottomRigth.parent());
    const QModelIndex &parent = topLeft.parent();
    if (parent.isValid() && !childrenVisible(parent)) {
        ASSERT_CONSISTENCY();
        return;
    }

    int topIndex = itemIndex(topLeft);
    if (topIndex == -1) // 'parent' is not visible anymore, though it's been expanded previously
        return;
    for (int i = topLeft.row(); i <= bottomRigth.row(); i++) {
        // Group items with same parent to minize the number of 'dataChanged()' emits
        int bottomIndex = topIndex;
        while (bottomIndex < m_items.count()) {
            const QModelIndex &idx = m_items.at(bottomIndex).index;
            if (idx.parent() != parent) {
                --bottomIndex;
                break;
            }
            if (idx.row() == bottomRigth.row())
                break;
            ++bottomIndex;
        }
        emit dataChanged(index(topIndex), index(bottomIndex), roles);

        i += bottomIndex - topIndex;
        if (i == bottomRigth.row())
            break;
        topIndex = bottomIndex + 1;
        while (topIndex < m_items.count()
               && m_items.at(topIndex).index.parent() != parent)
            topIndex++;
    }
    ASSERT_CONSISTENCY();
}

void QQuickTreeModelAdaptor1::modelLayoutAboutToBeChanged(const QList<QPersistentModelIndex> &parents, QAbstractItemModel::LayoutChangeHint hint)
{
    ASSERT_CONSISTENCY();
    Q_UNUSED(parents);
    Q_UNUSED(hint);
}

void QQuickTreeModelAdaptor1::modelLayoutChanged(const QList<QPersistentModelIndex> &parents, QAbstractItemModel::LayoutChangeHint hint)
{
    Q_UNUSED(hint);
    if (parents.isEmpty()) {
        m_items.clear();
        showModelTopLevelItems(false /*doInsertRows*/);
        emit dataChanged(index(0), index(m_items.count() - 1));
    }

    for (const QPersistentModelIndex &pmi : parents) {
        if (m_expandedItems.contains(pmi)) {
            int row = itemIndex(pmi);
            if (row != -1) {
                int rowCount = m_model->rowCount(pmi);
                if (rowCount > 0) {
                    const QModelIndex &lmi = m_model->index(rowCount - 1, 0, pmi);
                    int lastRow = lastChildIndex(lmi);
                    removeVisibleRows(row + 1, lastRow, false /*doRemoveRows*/);
                    showModelChildItems(m_items.at(row), 0, rowCount - 1, false /*doInsertRows*/);
                    emit dataChanged(index(row + 1), index(lastRow));
                }
            }
        }
    }
    ASSERT_CONSISTENCY();
}

void QQuickTreeModelAdaptor1::modelRowsAboutToBeInserted(const QModelIndex & parent, int start, int end)
{
    Q_UNUSED(parent);
    Q_UNUSED(start);
    Q_UNUSED(end);
    ASSERT_CONSISTENCY();
}

void QQuickTreeModelAdaptor1::modelRowsInserted(const QModelIndex & parent, int start, int end)
{
    TreeItem item;
    int parentRow = itemIndex(parent);
    if (parentRow >= 0) {
        const QModelIndex& parentIndex = index(parentRow);
        QVector<int> changedRole(1, HasChildrenRole);
        queueDataChanged(parentIndex, parentIndex, changedRole);
        item = m_items.at(parentRow);
        if (!item.expanded) {
            ASSERT_CONSISTENCY();
            return;
        }
    } else if (parent == m_rootIndex) {
        item = TreeItem(parent);
    } else {
        ASSERT_CONSISTENCY();
        return;
    }
    showModelChildItems(item, start, end);
    ASSERT_CONSISTENCY();
}

void QQuickTreeModelAdaptor1::modelRowsAboutToBeRemoved(const QModelIndex & parent, int start, int end)
{
    ASSERT_CONSISTENCY();
    enableSignalAggregation();
    if (parent == m_rootIndex || childrenVisible(parent)) {
        const QModelIndex &smi = m_model->index(start, 0, parent);
        int startIndex = itemIndex(smi);
        const QModelIndex &emi = m_model->index(end, 0, parent);
        int endIndex = -1;
        if (isExpanded(emi)) {
            int rowCount = m_model->rowCount(emi);
            if (rowCount > 0) {
                const QModelIndex &idx = m_model->index(rowCount - 1, 0, emi);
                endIndex = lastChildIndex(idx);
            }
        }
        if (endIndex == -1)
            endIndex = itemIndex(emi);

        removeVisibleRows(startIndex, endIndex);
    }

    for (int r = start; r <= end; r++) {
        const QModelIndex &cmi = m_model->index(r, 0, parent);
        m_expandedItems.remove(cmi);
    }
}

void QQuickTreeModelAdaptor1::modelRowsRemoved(const QModelIndex & parent, int start, int end)
{
    Q_UNUSED(start);
    Q_UNUSED(end);
    int parentRow = itemIndex(parent);
    if (parentRow >= 0) {
        const QModelIndex& parentIndex = index(parentRow);
        QVector<int> changedRole(1, HasChildrenRole);
        queueDataChanged(parentIndex, parentIndex, changedRole);
    }
    disableSignalAggregation();
    ASSERT_CONSISTENCY();
}

void QQuickTreeModelAdaptor1::modelRowsAboutToBeMoved(const QModelIndex & sourceParent, int sourceStart, int sourceEnd, const QModelIndex & destinationParent, int destinationRow)
{
    ASSERT_CONSISTENCY();
    enableSignalAggregation();
    m_visibleRowsMoved = false;
    if (!childrenVisible(sourceParent))
        return; // Do nothing now. See modelRowsMoved() below.

    if (!childrenVisible(destinationParent)) {
        modelRowsAboutToBeRemoved(sourceParent, sourceStart, sourceEnd);
        /* If the destination parent has no children, we'll need to
         * report a change on the HasChildrenRole */
        if (isVisible(destinationParent) && m_model->rowCount(destinationParent) == 0) {
            const QModelIndex &topLeft = index(itemIndex(destinationParent), 0, QModelIndex());
            const QModelIndex &bottomRight = topLeft;
            const QVector<int> changedRole(1, HasChildrenRole);
            queueDataChanged(topLeft, bottomRight, changedRole);
        }
    } else {
        int depthDifference = -1;
        if (destinationParent.isValid()) {
            int destParentIndex = itemIndex(destinationParent);
            depthDifference = m_items.at(destParentIndex).depth;
        }
        if (sourceParent.isValid()) {
            int sourceParentIndex = itemIndex(sourceParent);
            depthDifference -= m_items.at(sourceParentIndex).depth;
        } else {
            depthDifference++;
        }

        int startIndex = itemIndex(m_model->index(sourceStart, 0, sourceParent));
        const QModelIndex &emi = m_model->index(sourceEnd, 0, sourceParent);
        int endIndex = -1;
        if (isExpanded(emi)) {
            int rowCount = m_model->rowCount(emi);
            if (rowCount > 0)
                endIndex = lastChildIndex(m_model->index(rowCount - 1, 0, emi));
        }
        if (endIndex == -1)
            endIndex = itemIndex(emi);

        int destIndex = -1;
        if (destinationRow == m_model->rowCount(destinationParent)) {
            const QModelIndex &emi1 = m_model->index(destinationRow - 1, 0, destinationParent);
            destIndex = lastChildIndex(emi1) + 1;
        } else {
            destIndex = itemIndex(m_model->index(destinationRow, 0, destinationParent));
        }

        int totalMovedCount = endIndex - startIndex + 1;

        /* This beginMoveRows() is matched by a endMoveRows() in the
         * modelRowsMoved() method below. */
        m_visibleRowsMoved = startIndex != destIndex &&
            beginMoveRows(QModelIndex(), startIndex, endIndex, QModelIndex(), destIndex);

        const QList<TreeItem> &buffer = m_items.mid(startIndex, totalMovedCount);
        int bufferCopyOffset;
        if (destIndex > endIndex) {
            for (int i = endIndex + 1; i < destIndex; i++) {
                m_items.swapItemsAt(i, i - totalMovedCount); // Fast move from 1st to 2nd position
            }
            bufferCopyOffset = destIndex - totalMovedCount;
        } else {
            // NOTE: we will not enter this loop if startIndex == destIndex
            for (int i = startIndex - 1; i >= destIndex; i--) {
                m_items.swapItemsAt(i, i + totalMovedCount); // Fast move from 1st to 2nd position
            }
            bufferCopyOffset = destIndex;
        }
        for (int i = 0; i < buffer.length(); i++) {
            TreeItem item = buffer.at(i);
            item.depth += depthDifference;
            m_items.replace(bufferCopyOffset + i, item);
        }

        /* If both source and destination items are visible, the indexes of
         * all the items in between will change. If they share the same
         * parent, then this is all; however, if they belong to different
         * parents, their bottom siblings will also get displaced, so their
         * index also needs to be updated.
         * Given that the bottom siblings of the top moved elements are
         * already included in the update (since they lie between the
         * source and the dest elements), we only need to worry about the
         * siblings of the bottom moved element.
         */
        const int top = qMin(startIndex, bufferCopyOffset);
        int bottom = qMax(endIndex, bufferCopyOffset + totalMovedCount - 1);
        if (sourceParent != destinationParent) {
            const QModelIndex &bottomParent =
                bottom == endIndex ? sourceParent : destinationParent;

            const int rowCount = m_model->rowCount(bottomParent);
            if (rowCount > 0)
                bottom = qMax(bottom, lastChildIndex(m_model->index(rowCount - 1, 0, bottomParent)));
        }
        const QModelIndex &topLeft = index(top, 0, QModelIndex());
        const QModelIndex &bottomRight = index(bottom, 0, QModelIndex());
        const QVector<int> changedRole(1, ModelIndexRole);
        queueDataChanged(topLeft, bottomRight, changedRole);

        if (depthDifference != 0) {
            const QModelIndex &topLeft1 = index(bufferCopyOffset, 0, QModelIndex());
            const QModelIndex &bottomRight1 = index(bufferCopyOffset + totalMovedCount - 1, 0, QModelIndex());
            const QVector<int> changedRole1(1, DepthRole);
            queueDataChanged(topLeft1, bottomRight1, changedRole1);
        }
    }
}

void QQuickTreeModelAdaptor1::modelRowsMoved(const QModelIndex & sourceParent, int sourceStart, int sourceEnd, const QModelIndex & destinationParent, int destinationRow)
{
    if (!childrenVisible(sourceParent)) {
        modelRowsInserted(destinationParent, destinationRow, destinationRow + sourceEnd - sourceStart);
    } else if (!childrenVisible(destinationParent)) {
        modelRowsRemoved(sourceParent, sourceStart, sourceEnd);
    }

    if (m_visibleRowsMoved)
        endMoveRows();

    if (isVisible(sourceParent) && m_model->rowCount(sourceParent) == 0) {
        int parentRow = itemIndex(sourceParent);
        collapseRow(parentRow);
        const QModelIndex &topLeft = index(parentRow, 0, QModelIndex());
        const QModelIndex &bottomRight = topLeft;
        const QVector<int> changedRole { ExpandedRole, HasChildrenRole };
        queueDataChanged(topLeft, bottomRight, changedRole);
    }

    disableSignalAggregation();

    ASSERT_CONSISTENCY();
}

void QQuickTreeModelAdaptor1::dump() const
{
    if (!m_model)
        return;
    int count = m_items.count();
    if (count == 0)
        return;
    int countWidth = floor(log10(double(count))) + 1;
    qInfo() << "Dumping" << this;
    for (int i = 0; i < count; i++) {
        const TreeItem &item = m_items.at(i);
        bool hasChildren = m_model->hasChildren(item.index);
        int children = m_model->rowCount(item.index);
        qInfo().noquote().nospace()
                << QString("%1 ").arg(i, countWidth) << QString(4 * item.depth, QChar::fromLatin1('.'))
                << QLatin1String(!hasChildren ? ".. " : item.expanded ? " v " : " > ")
                << item.index << children;
    }
}

bool QQuickTreeModelAdaptor1::testConsistency(bool dumpOnFail) const
{
    if (!m_model) {
        if (!m_items.isEmpty()) {
            qWarning() << "Model inconsistency: No model but stored visible items";
            return false;
        }
        if (!m_expandedItems.isEmpty()) {
            qWarning() << "Model inconsistency: No model but stored expanded items";
            return false;
        }
        return true;
    }
    QModelIndex parent = m_rootIndex;
    QStack<QModelIndex> ancestors;
    QModelIndex idx = m_model->index(0, 0, parent);
    for (int i = 0; i < m_items.count(); i++) {
        bool isConsistent = true;
        const TreeItem &item = m_items.at(i);
        if (item.index != idx) {
            qWarning() << "QModelIndex inconsistency" << i << item.index;
            qWarning() << "    expected" << idx;
            isConsistent = false;
        }
        if (item.index.parent() != parent) {
            qWarning() << "Parent inconsistency" << i << item.index;
            qWarning() << "    stored index parent" << item.index.parent() << "model parent" << parent;
            isConsistent = false;
        }
        if (item.depth != ancestors.count()) {
            qWarning() << "Depth inconsistency" << i << item.index;
            qWarning() << "    item depth" << item.depth << "ancestors stack" << ancestors.count();
            isConsistent = false;
        }
        if (item.expanded && !m_expandedItems.contains(item.index)) {
            qWarning() << "Expanded inconsistency" << i << item.index;
            qWarning() << "    set" << m_expandedItems.contains(item.index) << "item" << item.expanded;
            isConsistent = false;
        }
        if (!isConsistent) {
            if (dumpOnFail)
                dump();
            return false;
        }
        QModelIndex firstChildIndex;
        if (item.expanded)
            firstChildIndex = m_model->index(0, 0, idx);
        if (firstChildIndex.isValid()) {
            ancestors.push(parent);
            parent = idx;
            idx = m_model->index(0, 0, parent);
        } else {
            while (idx.row() == m_model->rowCount(parent) - 1) {
                if (ancestors.isEmpty())
                    break;
                idx = parent;
                parent = ancestors.pop();
            }
            idx = m_model->index(idx.row() + 1, 0, parent);
        }
    }

    return true;
}

void QQuickTreeModelAdaptor1::enableSignalAggregation() {
    m_signalAggregatorStack++;
}

void QQuickTreeModelAdaptor1::disableSignalAggregation() {
    m_signalAggregatorStack--;
    Q_ASSERT(m_signalAggregatorStack >= 0);
    if (m_signalAggregatorStack == 0) {
        emitQueuedSignals();
    }
}

void QQuickTreeModelAdaptor1::queueDataChanged(const QModelIndex &topLeft,
                                               const QModelIndex &bottomRight,
                                               const QVector<int> &roles)
{
    if (isAggregatingSignals()) {
        m_queuedDataChanged.append(DataChangedParams { topLeft, bottomRight, roles });
    } else {
        emit dataChanged(topLeft, bottomRight, roles);
    }
}

void QQuickTreeModelAdaptor1::emitQueuedSignals()
{
    QVector<DataChangedParams> combinedUpdates;
    /* First, iterate through the queued updates and merge the overlapping ones
     * to reduce the number of updates.
     * We don't merge adjacent updates, because they are typically filed with a
     * different role (a parent row is next to its children).
     */
    for (const DataChangedParams &dataChange : m_queuedDataChanged) {
        int startRow = dataChange.topLeft.row();
        int endRow = dataChange.bottomRight.row();
        bool merged = false;
        for (DataChangedParams &combined : combinedUpdates) {
            int combinedStartRow = combined.topLeft.row();
            int combinedEndRow = combined.bottomRight.row();
            if ((startRow <= combinedStartRow && endRow >= combinedStartRow) ||
                (startRow <= combinedEndRow && endRow >= combinedEndRow)) {
                if (startRow < combinedStartRow) {
                    combined.topLeft = dataChange.topLeft;
                }
                if (endRow > combinedEndRow) {
                    combined.bottomRight = dataChange.bottomRight;
                }
                for (int role : dataChange.roles) {
                    if (!combined.roles.contains(role))
                        combined.roles.append(role);
                }
                merged = true;
                break;
            }
        }
        if (!merged) {
            combinedUpdates.append(dataChange);
        }
    }

    /* Finally, emit the dataChanged signals */
    for (const DataChangedParams &dataChange : combinedUpdates) {
        emit dataChanged(dataChange.topLeft, dataChange.bottomRight, dataChange.roles);
    }
    m_queuedDataChanged.clear();
}

QT_END_NAMESPACE
