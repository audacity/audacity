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
#ifndef MU_SETTINGLISTMODEL_H
#define MU_SETTINGLISTMODEL_H

#include <QAbstractListModel>
#include <QMap>

#include "settings.h"

namespace mu {
class SettingListModel : public QAbstractListModel
{
    Q_OBJECT
public:
    explicit SettingListModel(QObject* parent = nullptr);

    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QHash<int, QByteArray> roleNames() const override;

    Q_INVOKABLE void load();
    Q_INVOKABLE void changeVal(int index, QVariant newVal);

private:

    enum Roles {
        SectionRole = Qt::UserRole + 1,
        KeyRole,
        TypeRole,
        ValueRole,
        MinValueRole,
        MaxValueRole
    };

    QString typeToString(muse::Val::Type t) const;

    QList<muse::Settings::Item> m_items;
};
}

#endif // MU_SETTINGLISTMODEL_H
