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
#ifndef MU_UICOMPONENTS_ITEMMULTISELECTIONMODEL_H
#define MU_UICOMPONENTS_ITEMMULTISELECTIONMODEL_H

#include <QItemSelectionModel>

namespace mu::uicomponents {
class ItemMultiSelectionModel : public QItemSelectionModel
{
    Q_OBJECT

public:
    explicit ItemMultiSelectionModel(QAbstractItemModel* parent = nullptr);

    void setAllowedModifiers(Qt::KeyboardModifiers modifiers);
    void setSingleItemSelectionMode(bool on);

    QList<int> selectedRows() const;

    using QItemSelectionModel::select;

public slots:
    Q_INVOKABLE void select(const QModelIndex& index);

private:
    Qt::KeyboardModifiers m_allowedModifiers;
};
}

#endif // MU_UICOMPONENTS_ITEMMULTISELECTIONMODEL_H
