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
#ifndef MU_UICOMPONENTS_SELECTABLEITEMLISTMODEL_H
#define MU_UICOMPONENTS_SELECTABLEITEMLISTMODEL_H

#include <QAbstractListModel>

#include "itemmultiselectionmodel.h"

namespace mu::uicomponents {
class SelectableItemListModel : public QAbstractListModel
{
    Q_OBJECT

    Q_PROPERTY(int count READ rowCount NOTIFY countChanged)

    Q_PROPERTY(bool isMovingUpAvailable READ isMovingUpAvailable NOTIFY isMovingUpAvailableChanged)
    Q_PROPERTY(bool isMovingDownAvailable READ isMovingDownAvailable NOTIFY isMovingDownAvailableChanged)
    Q_PROPERTY(bool isRemovingAvailable READ isRemovingAvailable NOTIFY isRemovingAvailableChanged)

    Q_PROPERTY(bool hasSelection READ hasSelection NOTIFY selectionChanged)

public:
    class Item : public QObject
    {
    public:
        virtual ~Item() = default;

        explicit Item(QObject* parent = nullptr);
    };

    using ItemList = QList<Item*>;

    explicit SelectableItemListModel(QObject* parent = nullptr);

    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QHash<int, QByteArray> roleNames() const override;

    bool isMovingUpAvailable() const;
    bool isMovingDownAvailable() const;
    bool isRemovingAvailable() const;

    bool hasSelection() const;

    Q_INVOKABLE void selectRow(int row);
    Q_INVOKABLE void moveSelectionUp();
    Q_INVOKABLE void moveSelectionDown();
    Q_INVOKABLE void removeSelection();
    Q_INVOKABLE void clearSelection();

signals:
    void countChanged();
    void selectionChanged();

    void isMovingUpAvailableChanged(bool isMovingUpAvailable);
    void isMovingDownAvailableChanged(bool isMovingDownAvailable);
    void isRemovingAvailableChanged(bool isRemovingAvailable);

protected:
    enum Roles {
        RoleIsSelected = Qt::UserRole + 1,
        UserRole
    };

    virtual void onUpdateOperationsAvailability();
    virtual void onRowsMoved();
    virtual void onRowsRemoved();

    const ItemList& items() const;
    Item* item(const QModelIndex& index) const;

    void setItems(const ItemList& items);
    void insertItem(int row, Item* item);

    void setIsMovingUpAvailable(bool isMovingUpAvailable);
    void setIsMovingDownAvailable(bool isMovingDownAvailable);
    void setIsRemovingAvailable(bool isRemovingAvailable);

    ItemMultiSelectionModel* selection() const;

private:
    bool moveRows(const QModelIndex& sourceParent, int sourceRow, int count, const QModelIndex& destinationParent,
                  int destinationChild) override;

    bool isRowValid(int row) const;

    bool m_isMovingUpAvailable = false;
    bool m_isMovingDownAvailable = false;
    bool m_isRemovingAvailable = false;

    ItemMultiSelectionModel* m_selection = nullptr;
    ItemList m_items;
};
}

#endif // MU_UICOMPONENTS_SELECTABLEITEMLISTMODEL_H
