/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2023 MuseScore BVBA and others
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

#include "braillepreferencesmodel.h"

#include "translation.h"

using namespace au::appshell;
using namespace mu::braille;

BraillePreferencesModel::BraillePreferencesModel(QObject* parent)
    : QObject(parent)
{
}

bool BraillePreferencesModel::braillePanelEnabled() const
{
    return brailleConfiguration()->braillePanelEnabled();
}

QString BraillePreferencesModel::brailleTable() const
{
    return brailleConfiguration()->brailleTable();
}

int BraillePreferencesModel::intervalDirection() const
{
    return static_cast<int>(brailleConfiguration()->intervalDirection());
}

QStringList BraillePreferencesModel::brailleTables() const
{
    return brailleConfiguration()->brailleTableList();
}

QVariantList BraillePreferencesModel::intervalDirections() const
{
    return QVariantList {
        QVariantMap {
            //: Braille chord interval direction: automatic (based on clef)
            { "text", qtrc("appshell/preferences", "Auto") },
            { "value", static_cast<int>(BrailleIntervalDirection::Auto) },
        },
        QVariantMap {
            //: Braille chord interval direction: up (ascending)
            { "text", qtrc("appshell/preferences", "Up") },
            { "value", static_cast<int>(BrailleIntervalDirection::Up) },
        },
        QVariantMap {
            //: Braille chord interval direction: down (descending)
            { "text", qtrc("appshell/preferences", "Down") },
            { "value", static_cast<int>(BrailleIntervalDirection::Down) },
        },
    };
}

void BraillePreferencesModel::setBraillePanelEnabled(bool value)
{
    if (value == braillePanelEnabled()) {
        return;
    }

    brailleConfiguration()->setBraillePanelEnabled(value);
    emit braillePanelEnabledChanged(value);
}

void BraillePreferencesModel::setBrailleTable(QString table)
{
    if (table == brailleTable()) {
        return;
    }

    brailleConfiguration()->setBrailleTable(table);
    emit brailleTableChanged(table);
}

void BraillePreferencesModel::setIntervalDirection(int direction)
{
    if (direction == intervalDirection()) {
        return;
    }

    brailleConfiguration()->setIntervalDirection(static_cast<BrailleIntervalDirection>(direction));
    emit intervalDirectionChanged(direction);
}
