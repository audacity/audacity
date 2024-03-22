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

import MuseScore.Ui 1.0

FocusScope {
    id: root

    property bool checked: false
    property alias pressed: clickableArea.containsPress
    property alias hovered: clickableArea.containsMouse
    property bool isIndeterminate: false

    property alias text: label.text
    property alias font: label.font
    property alias backgroundColor: box.color
    property alias backgroundOpacity: box.opacity

    property alias navigation: navCtrl

    signal clicked

    implicitHeight: contentRow.implicitHeight
    implicitWidth: contentRow.implicitWidth

    opacity: root.enabled ? 1.0 : ui.theme.itemOpacityDisabled

    function ensureActiveFocus() {
        if (!root.activeFocus) {
            root.forceActiveFocus()
        }
    }

    NavigationControl {
        id: navCtrl

        name: root.objectName != "" ? root.objectName : "CheckBox"
        enabled: root.enabled && root.visible
        accessible.role: MUAccessible.CheckBox
        accessible.name: root.text
        accessible.checked: root.checked

        onActiveChanged: {
            if (!root.activeFocus) {
                root.forceActiveFocus()
            }
        }

        onTriggered: root.clicked()
    }

    RowLayout {
        id: contentRow
        spacing: 6

        Rectangle {
            id: box

            height: 20
            width: 20

            opacity: ui.theme.buttonOpacityNormal

            border.width: ui.theme.borderWidth
            border.color: ui.theme.strokeColor
            color: ui.theme.buttonColor

            radius: 2

            NavigationFocusBorder { navigationCtrl: navCtrl }

            StyledIconLabel {
                anchors.fill: parent
                iconCode: root.isIndeterminate ? IconCode.MINUS : IconCode.TICK_RIGHT_ANGLE
                visible: root.checked || root.isIndeterminate
            }
        }

        StyledTextLabel {
            id: label
            visible: !isEmpty

            readonly property real availableWidth: root.width - contentRow.spacing - box.width

            Layout.preferredWidth: availableWidth > 0 ? Math.min(availableWidth, label.implicitWidth) : label.implicitWidth
            Layout.alignment: Qt.AlignLeft | Qt.AlignVCenter

            horizontalAlignment: Text.AlignLeft
            wrapMode: Text.WordWrap
            maximumLineCount: 2
        }
    }

    MouseArea {
        id: clickableArea

        anchors.fill: contentRow
        anchors.margins: -4

        hoverEnabled: true

        onClicked: {
            navigation.requestActiveByInteraction()

            root.clicked()
        }
    }

    states: [
        State {
            name: "HOVERED"
            when: clickableArea.containsMouse && !clickableArea.pressed

            PropertyChanges {
                target: box
                opacity: ui.theme.buttonOpacityHover
            }
        },

        State {
            name: "PRESSED"
            when: clickableArea.containsMouse && clickableArea.pressed

            PropertyChanges {
                target: box
                opacity: ui.theme.buttonOpacityHit
            }
        }
    ]
}
