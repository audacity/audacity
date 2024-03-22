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
#ifndef MU_UICOMPONENTS_SORTFILTERPROXYMODEL_H
#define MU_UICOMPONENTS_SORTFILTERPROXYMODEL_H

#include <QSortFilterProxyModel>

#include "filtervalue.h"
#include "sortervalue.h"
#include "qmllistproperty.h"

namespace mu::uicomponents {
class SortFilterProxyModel : public QSortFilterProxyModel
{
    Q_OBJECT

    Q_PROPERTY(int rowCount READ rowCount NOTIFY rowCountChanged)

    Q_PROPERTY(QQmlListProperty<mu::uicomponents::FilterValue> filters READ filters CONSTANT)
    Q_PROPERTY(QQmlListProperty<mu::uicomponents::SorterValue> sorters READ sorters CONSTANT)
    Q_PROPERTY(QList<int> alwaysIncludeIndices READ alwaysIncludeIndices WRITE setAlwaysIncludeIndices NOTIFY alwaysIncludeIndicesChanged)
    Q_PROPERTY(QList<int> alwaysExcludeIndices READ alwaysExcludeIndices WRITE setAlwaysExcludeIndices NOTIFY alwaysExcludeIndicesChanged)

public:
    explicit SortFilterProxyModel(QObject* parent = nullptr);

    QQmlListProperty<FilterValue> filters();
    QQmlListProperty<SorterValue> sorters();

    QList<int> alwaysIncludeIndices() const;
    void setAlwaysIncludeIndices(const QList<int>& indices);

    QList<int> alwaysExcludeIndices() const;
    void setAlwaysExcludeIndices(const QList<int>& indices);

    QHash<int, QByteArray> roleNames() const override;

    void setSourceModel(QAbstractItemModel* sourceModel) override;

    Q_INVOKABLE void refresh();

signals:
    void rowCountChanged();

    void filtersChanged(QQmlListProperty<mu::uicomponents::FilterValue> filters);

    void alwaysIncludeIndicesChanged();
    void alwaysExcludeIndicesChanged();

    void sourceModelRoleNamesChanged();

protected:
    bool filterAcceptsRow(int sourceRow, const QModelIndex& sourceParent) const override;
    bool lessThan(const QModelIndex& left, const QModelIndex& right) const override;

private:
    void reset();
    void fillRoleIds();

    SorterValue* currentSorterValue() const;
    int roleKey(const QString& roleName) const;

    QmlListProperty<FilterValue> m_filters;
    QHash<int, FilterValue*> m_roleIdToFilterValueHash;

    QmlListProperty<SorterValue> m_sorters;

    QList<int> m_alwaysIncludeIndices;
    QList<int> m_alwaysExcludeIndices;

    QMetaObject::Connection m_subSourceModelConnection;
};
}

#endif // MU_UICOMPONENTS_SORTFILTERPROXYMODEL_H
