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
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

RowLayout {
    id: root

    height: childrenRect.height

    property bool isAddSeparatorAvailable: false
    property bool isRemovingAvailable: false
    property bool isMovingUpAvailable: false
    property bool isMovingDownAvailable: false

    signal addSeparatorLineRequested()
    signal removeSelectionRequested()
    signal moveSelectionUpRequested()
    signal moveSelectionDownRequested()

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "CustomiseControlPanel"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal

        //: Accessibility description of the button group at the top of the "Customize toolbar" popup
        accessible.name: qsTrc("projectscene", "Customization actions")

        onActiveChanged: function(active) {
            if (active) {
                root.forceActiveFocus()
            }
        }
    }

    FlatButton {
        Layout.fillWidth: true

        text: qsTrc("projectscene", "Add separator line")

        enabled: root.isAddSeparatorAvailable

        navigation.name: text
        navigation.panel: root.navigationPanel
        navigation.column: 0

        onClicked: {
            root.addSeparatorLineRequested()
        }
    }

    FlatButton {
        Layout.preferredWidth: 30

        icon: IconCode.DELETE_TANK
        enabled: root.isRemovingAvailable

        navigation.name: text
        navigation.panel: root.navigationPanel
        navigation.column: 1
        navigation.accessible.name: qsTrc("projectscene", "Delete")

        onClicked: {
            root.removeSelectionRequested()
        }
    }

    FlatButton {
        Layout.preferredWidth: 30

        icon: IconCode.ARROW_UP
        enabled: root.isMovingUpAvailable

        navigation.name: text
        navigation.panel: root.navigationPanel
        navigation.column: 2
        navigation.accessible.name: qsTrc("projectscene", "Move up")

        onClicked: {
            root.moveSelectionUpRequested()
        }
    }

    FlatButton {
        Layout.preferredWidth: 30

        icon: IconCode.ARROW_DOWN
        enabled: root.isMovingDownAvailable

        navigation.name: text
        navigation.panel: root.navigationPanel
        navigation.column: 3
        navigation.accessible.name: qsTrc("projectscene", "Move down")

        onClicked: {
            root.moveSelectionDownRequested()
        }
    }
}
