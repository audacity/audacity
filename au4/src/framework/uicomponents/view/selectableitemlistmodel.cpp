/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include "selectableitemlistmodel.h"

#include "log.h"

using namespace mu::uicomponents;

SelectableItemListModel::Item::Item(QObject* parent)
    : QObject(parent)
{
}

SelectableItemListModel::SelectableItemListModel(QObject* parent)
    : QAbstractListModel(parent), m_selection(new ItemMultiSelectionModel(this))
{
    connect(m_selection, &ItemMultiSelectionModel::selectionChanged, [this]() {
        emit selectionChanged();
        onUpdateOperationsAvailability();
    });
}

QVariant SelectableItemListModel::data(const QModelIndex& index, int role) const
{
    switch (role) {
    case RoleIsSelected: return m_selection->isSelected(index);
    }

    return QVariant();
}

int SelectableItemListModel::rowCount(const QModelIndex&) const
{
    return m_items.size();
}

QHash<int, QByteArray> SelectableItemListModel::roleNames() const
{
    static const QHash<int, QByteArray> roles {
        { RoleIsSelected, "isSelected" }
    };

    return roles;
}

bool SelectableItemListModel::isMovingUpAvailable() const
{
    return m_isMovingUpAvailable;
}

bool SelectableItemListModel::isMovingDownAvailable() const
{
    return m_isMovingDownAvailable;
}

bool SelectableItemListModel::isRemovingAvailable() const
{
    return m_isRemovingAvailable;
}

bool SelectableItemListModel::hasSelection() const
{
    return m_selection->hasSelection();
}

void SelectableItemListModel::selectRow(int row)
{
    TRACEFUNC;

    m_selection->select(index(row));

    emit dataChanged(index(0), index(rowCount() - 1), { RoleIsSelected });
}

void SelectableItemListModel::moveSelectionUp()
{
    TRACEFUNC;

    QModelIndexList selectedIndexList = m_selection->selectedIndexes();

    if (selectedIndexList.isEmpty()) {
        return;
    }

    std::sort(selectedIndexList.begin(), selectedIndexList.end(), [](const QModelIndex& f, const QModelIndex& s) -> bool {
        return f.row() < s.row();
    });

    QModelIndex sourceRowFirst = selectedIndexList.first();

    moveRows(sourceRowFirst.parent(), sourceRowFirst.row(), selectedIndexList.count(), sourceRowFirst.parent(), sourceRowFirst.row() - 1);
}

void SelectableItemListModel::moveSelectionDown()
{
    TRACEFUNC;

    QModelIndexList selectedIndexList = m_selection->selectedIndexes();

    if (selectedIndexList.isEmpty()) {
        return;
    }

    std::sort(selectedIndexList.begin(), selectedIndexList.end(), [](const QModelIndex& f, const QModelIndex& s) -> bool {
        return f.row() < s.row();
    });

    QModelIndex sourceRowFirst = selectedIndexList.first();
    QModelIndex sourceRowLast = selectedIndexList.last();

    moveRows(sourceRowFirst.parent(), sourceRowFirst.row(), selectedIndexList.count(), sourceRowFirst.parent(), sourceRowLast.row() + 1);
}

void SelectableItemListModel::removeSelection()
{
    TRACEFUNC;

    if (!isRemovingAvailable()) {
        return;
    }

    QList<int> selectedRows = m_selection->selectedRows();
    if (selectedRows.isEmpty()) {
        return;
    }

    ItemList itemsToRemove;
    for (int row : selectedRows) {
        itemsToRemove << m_items[row];
    }

    beginResetModel();
    m_selection->clear();

    for (auto item : itemsToRemove) {
        m_items.removeOne(item);
    }

    endResetModel();

    emit countChanged();

    onRowsRemoved();
}

void SelectableItemListModel::clearSelection()
{
    m_selection->clearSelection();
}

bool SelectableItemListModel::moveRows(const QModelIndex& sourceParent,
                                       int sourceRow,
                                       int count,
                                       const QModelIndex& destinationParent,
                                       int destinationChild)
{
    TRACEFUNC;

    int sourceFirstRow = sourceRow;
    int sourceLastRow = sourceRow + count - 1;
    int destinationRow = (sourceLastRow > destinationChild) ? destinationChild : destinationChild + 1;

    beginMoveRows(sourceParent, sourceFirstRow, sourceLastRow, destinationParent, destinationRow);

    int increaseCount = (sourceRow > destinationChild) ? 1 : 0;
    int moveIndex = 0;
    for (int i = 0; i < count; i++) {
        m_items.move(sourceRow + moveIndex, destinationChild + moveIndex);
        moveIndex += increaseCount;
    }

    endMoveRows();

    onUpdateOperationsAvailability();
    onRowsMoved();

    return true;
}

bool SelectableItemListModel::isRowValid(int row) const
{
    return row >= 0 && row < m_items.size();
}

void SelectableItemListModel::onUpdateOperationsAvailability()
{
    TRACEFUNC;

    QList<int> rows = m_selection->selectedRows();
    bool hasSelection = !rows.isEmpty();

    bool isMovingUpAvailable = hasSelection && rows.first() > 0;
    bool isMovingDownAvailable = hasSelection && rows.last() < m_items.size() - 1;
    bool isRemovingAvailable = hasSelection;

    setIsMovingUpAvailable(isMovingUpAvailable);
    setIsMovingDownAvailable(isMovingDownAvailable);
    setIsRemovingAvailable(isRemovingAvailable);
}

void SelectableItemListModel::onRowsMoved()
{
}

void SelectableItemListModel::onRowsRemoved()
{
}

const SelectableItemListModel::ItemList& SelectableItemListModel::items() const
{
    return m_items;
}

SelectableItemListModel::Item* SelectableItemListModel::item(const QModelIndex& index) const
{
    int row = index.row();

    if (!isRowValid(row)) {
        return nullptr;
    }

    return m_items[row];
}

void SelectableItemListModel::setItems(const ItemList& items)
{
    TRACEFUNC;

    beginResetModel();

    m_selection->clear();
    m_items = items;

    endResetModel();

    emit countChanged();
}

void SelectableItemListModel::insertItem(int row, Item* item)
{
    TRACEFUNC;

    beginInsertRows(QModelIndex(), row, row);
    m_items.insert(row, item);
    endInsertRows();
}

void SelectableItemListModel::setIsMovingUpAvailable(bool isMovingUpAvailable)
{
    if (m_isMovingUpAvailable == isMovingUpAvailable) {
        return;
    }

    m_isMovingUpAvailable = isMovingUpAvailable;
    emit isMovingUpAvailableChanged(m_isMovingUpAvailable);
}

void SelectableItemListModel::setIsMovingDownAvailable(bool isMovingDownAvailable)
{
    if (m_isMovingDownAvailable == isMovingDownAvailable) {
        return;
    }

    m_isMovingDownAvailable = isMovingDownAvailable;
    emit isMovingDownAvailableChanged(m_isMovingDownAvailable);
}

void SelectableItemListModel::setIsRemovingAvailable(bool isRemovingAvailable)
{
    if (m_isRemovingAvailable == isRemovingAvailable) {
        return;
    }

    m_isRemovingAvailable = isRemovingAvailable;
    emit isRemovingAvailableChanged(m_isRemovingAvailable);
}

ItemMultiSelectionModel* SelectableItemListModel::selection() const
{
    return m_selection;
}
