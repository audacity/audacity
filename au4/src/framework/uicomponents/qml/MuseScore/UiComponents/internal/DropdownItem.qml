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

import MuseScore.Ui 1.0

import ".."

Item {

    id: root

    property alias text: labelItem.text

    property bool selected: false

    property bool insideDropdownList: false
    property alias background: backgroundItem
    property alias label: labelItem
    property alias mouseArea: mouseAreaItem

    property color hoveredColor: backgroundItem.color

    property alias navigation: navCtrl

    signal clicked()

    NavigationControl {
        id: navCtrl

        name: root.objectName != "" ? root.objectName : "Dropdown"
        enabled: root.enabled && root.visible
        accessible.role: MUAccessible.ListItem
        accessible.name: root.text

        onActiveChanged: {
            if (!root.activeFocus) {
                root.forceActiveFocus()
            }
        }

        onTriggered: root.clicked()
    }

    Rectangle {
        id: backgroundItem
        anchors.fill: parent
        color: ui.theme.buttonColor
        radius: 3
        opacity: 0.7

        NavigationFocusBorder { navigationCtrl: navCtrl }
    }

    StyledTextLabel {
        id: labelItem
        anchors.fill: parent
        anchors.leftMargin: 12
        horizontalAlignment: Text.AlignLeft
    }

    MouseArea {
        id: mouseAreaItem
        anchors.fill: parent
        hoverEnabled: true
        onClicked: root.clicked()

        onContainsMouseChanged: {
            if (!labelItem.truncated) {
                return
            }

            if (mouseArea.containsMouse) {
                ui.tooltip.show(root, labelItem.text)
            } else {
                ui.tooltip.hide(root)
            }
        }
    }

    states: [
        State {
            name: "FOCUSED_INSIDE_DROPDOWN"
            when: root.insideDropdownList && navCtrl.active

            PropertyChanges {
                target: backgroundItem
                anchors.margins: ui.theme.navCtrlBorderWidth //this effectively cancels its child's margins and draws everything inside
            }
        },

        State {
            name: "HOVERED"
            when: mouseAreaItem.containsMouse && !mouseAreaItem.pressed

            PropertyChanges {
                target: backgroundItem
                opacity: ui.theme.buttonOpacityHover
                color: root.hoveredColor
            }
        },

        State {
            name: "PRESSED"
            when: mouseAreaItem.pressed

            PropertyChanges {
                target: backgroundItem
                opacity: ui.theme.buttonOpacityHit
            }
        },

        State {
            name: "SELECTED"
            when: root.selected

            PropertyChanges {
                target: backgroundItem
                opacity: ui.theme.accentOpacityHit
                color: ui.theme.accentColor
            }
        }
    ]
}
