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
import QtQuick.Layouts 1.12

import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0

Row {
    id: root

    property alias canEditAction: editActionButton.enabled

    spacing: 8

    signal editActionRequested()
    signal clearSelectedActionsRequested()
    signal clearAllActionsRequested()

    property NavigationPanel navigation: NavigationPanel {
        name: "MidiMappingBottomPanel"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        accessible.name: qsTrc("shortcuts", "MIDI mapping bottom panel")

        onActiveChanged: function(active) {
            if (active) {
                root.forceActiveFocus()
            }
        }
    }

    FlatButton {
        id: editActionButton

        text: qsTrc("shortcuts", "Assign MIDI mapping…")

        navigation.name: "EditActionButton"
        navigation.panel: root.navigation
        navigation.column: 0

        onClicked: {
            root.editActionRequested()
        }
    }

    FlatButton {
        width: 100

        text: qsTrc("global", "Clear")

        navigation.name: "ClearSelectedButton"
        navigation.panel: root.navigation
        navigation.column: 1

        onClicked: {
            root.clearSelectedActionsRequested()
        }
    }

    FlatButton {
        width: 100

        text: qsTrc("global", "Clear all")

        navigation.name: "ClearAllButton"
        navigation.panel: root.navigation
        navigation.column: 2

        onClicked: {
            root.clearAllActionsRequested()
        }
    }
}
