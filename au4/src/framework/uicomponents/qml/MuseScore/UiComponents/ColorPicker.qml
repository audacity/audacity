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

import "Utils.js" as Utils

Rectangle {
    id: root

    property bool isIndeterminate: false

    property alias navigation: navCtrl

    signal newColorSelected(var newColor)

    height: 30
    width: parent.width

    opacity: enabled ? 1 : ui.theme.itemOpacityDisabled

    radius: 3
    color: "#000000"

    NavigationFocusBorder { navigationCtrl: navCtrl }

    border.width: ui.theme.borderWidth
    border.color: ui.theme.strokeColor

    ColorPickerModel {
        id: colorPickerModel
    }

    QtObject {
        id: prv

        function selectColor() {
            var selectedColor = colorPickerModel.selectColor(root.color)
            root.newColorSelected(selectedColor)
        }
    }

    NavigationControl {
        id: navCtrl
        name: root.objectName != "" ? root.objectName : "ColorPicker"
        enabled: root.enabled && root.visible
        accessible.role: MUAccessible.Button
        accessible.name: Utils.accessibleColorDescription(root.color)

        onTriggered: {
            prv.selectColor()
        }
    }

    StyledIconLabel {
        anchors.fill: parent
        iconCode: IconCode.QUESTION_MARK
        visible: isIndeterminate
    }

    MouseArea {
        id: clickableArea

        anchors.fill: parent
        hoverEnabled: true
        onClicked: {
            navigation.requestActiveByInteraction()

            prv.selectColor()
        }
    }

    states: [
        State {
            name: "HOVERED"
            when: clickableArea.containsMouse && !clickableArea.pressed

            PropertyChanges { target: root; border.color: ui.theme.accentColor }
        },

        State {
            name: "PRESSED"
            when: clickableArea.pressed

            PropertyChanges { target: root; border.color: ui.theme.fontPrimaryColor }
        }
    ]
}

