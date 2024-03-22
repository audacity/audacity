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
import MuseScore.UiComponents 1.0

FocusScope {
    id: root

    property alias navigation: navCtrl

    property alias icon: buttonIcon.iconCode
    property bool checked: false

    property string toolTipTitle: ""
    property string toolTipDescription: ""
    property string toolTipShortcut: ""

    property bool transparent: false
    property color normalColor: transparent ? "transparent" : ui.theme.buttonColor
    property color hoverHitColor: ui.theme.buttonColor
    property color checkedColor: ui.theme.accentColor

    signal toggled()

    implicitHeight: 30
    implicitWidth: 30

    opacity: root.enabled ? 1.0 : ui.theme.itemOpacityDisabled

    NavigationControl {
        id: navCtrl
        name: root.objectName != "" ? root.objectName : "FlatToggleButton"
        enabled: root.enabled && root.visible

        accessible.role: MUAccessible.RadioButton
        accessible.checked: root.checked

        onTriggered: root.toggled()
    }

    Rectangle {
        id: backgroundRect
        anchors.fill: parent

        NavigationFocusBorder { navigationCtrl: navCtrl }

        color: root.checked ? root.checkedColor : root.normalColor
        opacity: ui.theme.buttonOpacityNormal

        border.width: ui.theme.borderWidth
        border.color: ui.theme.strokeColor
        radius: 2
    }

    StyledIconLabel {
        id: buttonIcon
        anchors.fill: parent
    }

    MouseArea {
        id: mouseArea
        anchors.fill: parent

        hoverEnabled: true
        onClicked: {
            navigation.requestActiveByInteraction()
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

    states: [
        State {
            name: "HOVERED"
            when: mouseArea.containsMouse && !mouseArea.pressed

            PropertyChanges {
                target: backgroundRect
                color: root.checked ? root.checkedColor : root.hoverHitColor
                opacity: ui.theme.buttonOpacityHover
            }
        },

        State {
            name: "PRESSED"
            when: mouseArea.pressed

            PropertyChanges {
                target: backgroundRect
                color: root.checked ? root.checkedColor : root.hoverHitColor
                opacity: ui.theme.buttonOpacityHit
            }
        }
    ]
}
