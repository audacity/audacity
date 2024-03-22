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
import QtQuick 2.15

import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0

FocusScope {
    id: root

    property alias navigation: navCtrl

    property bool checked: false

    property string toolTipTitle: ""
    property string toolTipDescription: ""
    property string toolTipShortcut: ""

    signal toggled()

    implicitHeight: 20
    implicitWidth: 36

    opacity: root.enabled ? 1.0 : ui.theme.itemOpacityDisabled

    function ensureActiveFocus() {
        if (!root.activeFocus) {
            root.forceActiveFocus()
        }
    }

    NavigationControl {
        id: navCtrl
        name: root.objectName != "" ? root.objectName : "ToggleButton"
        enabled: root.enabled && root.visible

        accessible.role: MUAccessible.CheckBox
        accessible.checked: root.checked

        onTriggered: root.toggled()
    }

    Rectangle {
        id: backgroundRect

        anchors.fill: parent

        color: root.checked ? ui.theme.accentColor : ui.theme.buttonColor

        border.width: ui.theme.borderWidth
        border.color: ui.theme.strokeColor
        radius: root.height / 2

        NavigationFocusBorder {
            navigationCtrl: navCtrl
        }

        Rectangle {
            id: handleRect

            readonly property int margins: 2

            anchors.verticalCenter: parent.verticalCenter
            x: root.checked ? parent.width - width - margins : margins

            width: root.height - handleRect.margins * 2
            height: width
            radius: width / 2

            color: "#FFFFFF"
        }

        StyledDropShadow {
            anchors.fill: handleRect
            source: handleRect

            color: "#33000000"
            radius: 4
            verticalOffset: 1
        }
    }

    MouseArea {
        id: mouseArea
        anchors.fill: parent

        hoverEnabled: true
        onClicked: {
            navigation.requestActiveByInteraction()

            root.ensureActiveFocus()
            root.toggled()
        }

        onPressed: {
            ui.tooltip.hide(root, true)
        }

        onContainsMouseChanged: {
            if (!Boolean(root.toolTipTitle)) {
                return
            }

            if (mouseArea.containsMouse) {
                ui.tooltip.show(root, root.toolTipTitle, root.toolTipDescription, root.toolTipShortcut)
            } else {
                ui.tooltip.hide(root)
            }
        }
    }
}
