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
import QtQuick.Controls 2.15

import Muse.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Miscellaneous")

    property alias selectionProximity: selectionProximityControl.currentValue

    signal selectionProximityChangeRequested(int proximity)

    IncrementalPropertyControlWithTitle {
        id: selectionProximityControl

        title: qsTrc("appshell/preferences", "Proximity for selecting elements:")

        columnWidth: root.columnWidth
        spacing: root.columnSpacing
        control.width: 60

        minValue: 1
        maxValue: 99
        measureUnitsSymbol: "px"

        navigation.name: "SelectionProximityControl"
        navigation.panel: root.navigation
        navigation.row: 1
        navigation.column: 0

        onValueEdited: function(newValue) {
            root.selectionProximityChangeRequested(newValue)
        }
    }
}
