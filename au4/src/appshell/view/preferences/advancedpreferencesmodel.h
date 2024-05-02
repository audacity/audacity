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
#ifndef AU_APPSHELL_ADVANCEDPREFERENCESMODEL_H
#define AU_APPSHELL_ADVANCEDPREFERENCESMODEL_H

#include <QAbstractListModel>

#include "settings.h"

namespace au::appshell {
class AdvancedPreferencesModel : public QAbstractListModel
{
    Q_OBJECT

public:
    explicit AdvancedPreferencesModel(QObject* parent = nullptr);

    QVariant data(const QModelIndex& index, int role) const override;
    bool setData(const QModelIndex& index, const QVariant& value, int role) override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QHash<int, QByteArray> roleNames() const override;

    Q_INVOKABLE void load();
    Q_INVOKABLE void resetToDefault();

private:

    enum Roles {
        KeyRole = Qt::UserRole + 1,
        DescriptionRole,
        TypeRole,
        ValueRole,
        MinValueRole,
        MaxValueRole
    };

    void changeVal(int index, QVariant newVal);
    QString typeToString(Val::Type type) const;

    QList<Settings::Item> m_items;
};
}

#endif // AU_APPSHELL_ADVANCEDPREFERENCESMODEL_H
