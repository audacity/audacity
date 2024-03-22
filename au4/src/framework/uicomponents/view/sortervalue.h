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
#ifndef MU_UICOMPONENTS_SORTERVALUE_H
#define MU_UICOMPONENTS_SORTERVALUE_H

#include <QObject>

namespace mu::uicomponents {
class SorterValue : public QObject
{
    Q_OBJECT

    Q_PROPERTY(QString roleName READ roleName WRITE setRoleName NOTIFY dataChanged)
    Q_PROPERTY(Qt::SortOrder sortOrder READ sortOrder WRITE setSortOrder NOTIFY dataChanged)
    Q_PROPERTY(bool enabled READ enabled WRITE setEnabled NOTIFY dataChanged)

public:
    explicit SorterValue(QObject* parent = nullptr);

    QString roleName() const;
    Qt::SortOrder sortOrder() const;
    bool enabled() const;

public slots:
    void setRoleName(QString roleName);
    void setSortOrder(Qt::SortOrder sortOrder);
    void setEnabled(bool enabled);

signals:
    void dataChanged();

private:
    QString m_roleName;
    Qt::SortOrder m_sortOrder = Qt::SortOrder::AscendingOrder;
    bool m_enabled = false;
};
}

#endif // MU_UICOMPONENTS_SORTERVALUE_H
