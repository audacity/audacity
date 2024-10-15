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

import Muse.Ui 1.0
import Muse.UiComponents 1.0

RadioButtonGroup {
    id: root

    property alias colors: root.model
    property alias currentColorIndex: root.currentIndex

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "AccentColorsList"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal

        onNavigationEvent: function(event) {
            if (event.type === NavigationEvent.AboutActive) {
                event.setData("controlIndex", [navigationRow, navigationColumnStart + root.currentIndex])
            }
        }
    }

    property int navigationRow: -1
    property int navigationColumnStart: 0
    property int navigationColumnEnd: navigationColumnStart + count

    property real sampleSize: 30
    readonly property real totalSampleSize: sampleSize + 6

    signal accentColorChangeRequested(var newColorIndex)

    implicitWidth: count * totalSampleSize + (count - 1) * spacing
    implicitHeight: totalSampleSize
    spacing: 10

    delegate: RoundedRadioButton {
        id: button

        width: root.totalSampleSize
        height: width

        checked: root.currentIndex === model.index

        property color accentColor: modelData

        navigation.name: "AccentColourButton"
        navigation.panel: root.navigationPanel
        navigation.row: root.navigationRow
        navigation.column: root.navigationColumnStart + model.index
        navigation.accessible.name: Utils.accessibleColorDescription(accentColor)

        onToggled: {
            root.accentColorChangeRequested(model.index)
        }

        indicator: Rectangle {
            anchors.fill: parent

            color: "transparent"
            border.color: ui.theme.fontPrimaryColor
            border.width: parent.checked ? 1 : 0
            radius: width / 2

            NavigationFocusBorder { navigationCtrl: button.navigation }

            Rectangle {
                anchors.centerIn: parent

                width: root.sampleSize
                height: width
                radius: width / 2

                border.color: ui.theme.strokeColor
                border.width: 1

                color: button.accentColor
            }
        }

        background: Item {}
    }
}
