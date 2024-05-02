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
import QtQuick 2.15

import Muse.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Advanced options")

    property var tables: null
    property string brailleTable: ""

    property var directions: null
    property int intervalDirection: -1

    signal brailleTableChangeRequested(string table)
    signal intervalDirectionChangeRequested(int direction)

    ComboBoxWithTitle {
        title: qsTrc("appshell/preferences", "Braille table for lyrics:")
        columnWidth: root.columnWidth

        currentIndex: control.indexOfValue(root.brailleTable)
        model: root.tables

        navigation.name: "BrailleTableBox"
        navigation.panel: root.navigation
        navigation.row: 1

        onValueEdited: function(newIndex, newValue) {
            root.brailleTableChangeRequested(newValue);
        }
    }

    ComboBoxWithTitle {
        title: qsTrc("appshell/preferences", "Interval direction:")
        columnWidth: root.columnWidth

        currentIndex: control.indexOfValue(root.intervalDirection)
        model: root.directions

        navigation.name: "IntervalDirectionBox"
        navigation.panel: root.navigation
        navigation.row: 2

        onValueEdited: function(newIndex, newValue) {
            root.intervalDirectionChangeRequested(newValue);
        }
    }
}
