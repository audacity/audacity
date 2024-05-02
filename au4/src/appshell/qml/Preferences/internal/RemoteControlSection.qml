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

import Muse.Ui 1.0
import Muse.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "OSC remote control")

    navigation.direction: NavigationPanel.Horizontal

    property alias isOSCRemoteControl: isOSCRemoteControlCheckBox.checked
    property alias oscPort: oscPortControl.currentValue

    signal remoteControlChanged(bool control)
    signal portChanged(int port)

    Row {
        spacing: 12

        CheckBox {
            id: isOSCRemoteControlCheckBox

            width: root.columnWidth
            anchors.verticalCenter: parent.verticalCenter

            text: qsTrc("appshell/preferences", "Port number:")

            navigation.name: "RemoteControlCheckBox"
            navigation.panel: root.navigation
            navigation.column: 1

            onClicked: {
                root.remoteControlChanged(!checked)
            }
        }

        IncrementalPropertyControl {
            id: oscPortControl

            width: 96
            anchors.verticalCenter: parent.verticalCenter

            enabled: root.isOSCRemoteControl

            minValue: 1
            maxValue: 65535
            step: 1
            decimals: 0

            navigation.name: "OscPortControl"
            navigation.panel: root.navigation
            navigation.column: 2

            onValueEdited: function(newValue) {
                root.portChanged(newValue)
            }
        }
    }
}
