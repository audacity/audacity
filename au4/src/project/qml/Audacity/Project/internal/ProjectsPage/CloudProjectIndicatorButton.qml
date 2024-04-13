/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * Music Composition & Notation
 *
 * Copyright (C) 2024 Audacity BVBA and others
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
import Muse.Ui 1.0

Item {
    id: root

    property bool isProgress: false
    property bool isDownloadedAndUpToDate: false

    property alias navigation: navCtrl

    signal clicked

    width: 26
    height: 26

    QtObject {
        id: prv

        readonly property string toolTipText: root.isProgress ? qsTrc("project", "Stop download") : qsTrc("project", "Download project")
    }

    NavigationControl {
        id: navCtrl

        name: root.objectName !== "" ? root.objectName : "CloudProjectIndicatorButton"
        enabled: root.enabled && root.visible

        accessible.role: MUAccessible.Button
        accessible.name: prv.toolTipText
        accessible.visualItem: root
        accessible.enabled: navCtrl.enabled

        onTriggered: {
            if (navCtrl.enabled) {
                root.clicked()
            }
        }
    }

    Rectangle {
        id: background
        anchors.fill: parent
        radius: width / 2
        color: "white"
        opacity: 0.6
    }

    Rectangle {
        id: foreground
        anchors.fill: parent
        radius: width / 2

        NavigationFocusBorder { navigationCtrl: navCtrl }
    }

    states: [
        State {
            name: "downloaded and up to date"
            when: root.isDownloadedAndUpToDate && !root.isProgress

            PropertyChanges {
                target: background
                visible: false
            }

            PropertyChanges {
                target: foreground
                color: ui.theme.accentColor
            }
        },

        State {
            name: "pressed"
            when: mouseArea.pressed

            PropertyChanges {
                target: foreground
                color: Utils.colorWithAlpha("black", 0.8)
                border.color: Utils.colorWithAlpha("white", 0.9)
                border.width: 1
            }
        },

        State {
            name: "hovered"
            when: mouseArea.containsMouse && !mouseArea.pressed

            PropertyChanges {
                target: foreground
                color: Utils.colorWithAlpha("black", 0.5)
                border.color: Utils.colorWithAlpha("white", 0.9)
                border.width: 1
            }
        },

        State {
            name: "normal"
            when: true

            PropertyChanges {
                target: foreground
                color: Utils.colorWithAlpha("black", 0.65)
                border.color: Utils.colorWithAlpha("white", 0.9)
                border.width: 1
            }
        }
    ]


    ItemWithDropShadow {
        anchors.centerIn: parent

        implicitWidth: icon.implicitWidth
        implicitHeight: icon.implicitHeight

        shadow.horizontalOffset: 0
        shadow.verticalOffset: 1
        shadow.radius: 4

        StyledIconLabel {
            id: icon

            iconCode: root.isProgress ? IconCode.STOP_FILL : IconCode.CLOUD_FILL
            font.pixelSize: root.isProgress ? 12 : 14
            color: "white"
        }
    }

    MouseArea {
        id: mouseArea
        anchors.fill: root

        hoverEnabled: true

        onContainsMouseChanged: {
            if (mouseArea.containsMouse) {
                ui.tooltip.show(root, prv.toolTipText)
            } else {
                ui.tooltip.hide(root)
            }
        }

        onPressed: {
            ui.tooltip.hide(root, true)
        }

        onClicked: {
            root.clicked()
        }
    }
}
