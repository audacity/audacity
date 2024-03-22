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

import MuseScore.Ui 1.0

RadioDelegate {
    id: root

    default property Component contentComponent: null

    //! NOTE Don't use the `icon` property.
    //!      It's a property of the ancestor of RadioDelegate
    //!      and has the wrong type (QQuickIcon).
    //!      It can't be overridden either, as it is marked `FINAL`.
    property int iconCode: IconCode.NONE
    property int iconFontSize: ui.theme.iconsFont.pixelSize

    property string toolTipTitle: ""
    property string toolTipDescription: ""
    property string toolTipShortcut: ""

    property alias radius: backgroundRect.radius

    property bool transparent: false
    property color normalColor: transparent ? "transparent" : ui.theme.buttonColor
    property color hoverHitColor: ui.theme.buttonColor
    property color checkedColor: ui.theme.accentColor

    property alias navigation: navCtrl
    property alias navigationFocusBorder: navigationFocusBorder

    ButtonGroup.group: ListView.view && ListView.view instanceof RadioButtonGroup ? ListView.view.radioButtonGroup : null

    implicitHeight: ListView.view ? ListView.view.height : ui.theme.defaultButtonSize
    implicitWidth: ListView.view ? (ListView.view.width - (ListView.view.spacing * (ListView.view.count - 1))) / ListView.view.count
                                 : ui.theme.defaultButtonSize
    hoverEnabled: true

    onClicked: {
        navigation.requestActiveByInteraction()
    }

    onPressedChanged: {
        ui.tooltip.hide(root, true)
    }

    onHoveredChanged: {
        if (!Boolean(root.toolTipTitle)) {
            return
        }

        if (hovered) {
            ui.tooltip.show(root, root.toolTipTitle, root.toolTipDescription, root.toolTipShortcut)
        } else {
            ui.tooltip.hide(root)
        }
    }

    NavigationControl {
        id: navCtrl
        name: root.objectName != "" ? root.objectName : "FlatRadioButton"
        enabled: root.enabled && root.visible

        accessible.role: MUAccessible.RadioButton
        accessible.name: root.text
        accessible.checked: root.checked

        onTriggered: root.toggled()
    }

    background: Rectangle {
        id: backgroundRect
        anchors.fill: parent

        NavigationFocusBorder {
            id: navigationFocusBorder
            navigationCtrl: navCtrl
        }

        color: root.checked ? root.checkedColor : root.normalColor
        opacity: ui.theme.buttonOpacityNormal

        border.width: ui.theme.borderWidth
        border.color: ui.theme.strokeColor
        radius: 2

        states: [
            State {
                name: "HOVERED"
                when: root.hovered && !root.pressed

                PropertyChanges {
                    target: backgroundRect
                    color: root.checked ? root.checkedColor : root.hoverHitColor
                    opacity: ui.theme.buttonOpacityHover
                }
            },

            State {
                name: "PRESSED"
                when: root.pressed

                PropertyChanges {
                    target: backgroundRect
                    color: root.checked ? root.checkedColor : root.hoverHitColor
                    opacity: ui.theme.buttonOpacityHit
                }
            }
        ]
    }

    contentItem: Loader {
        id: contentLoader
        anchors.fill: parent

        sourceComponent: {
            if (root.contentComponent) {
                return root.contentComponent
            }

            if (root.iconCode && root.iconCode !== IconCode.NONE) {
                return iconComponent
            }

            return textComponent
        }

        Component {
            id: iconComponent

            StyledIconLabel {
                iconCode: root.iconCode
                font.pixelSize: root.iconFontSize
            }
        }

        Component {
            id: textComponent

            StyledTextLabel {
                text: root.text
                maximumLineCount: 1
            }
        }
    }

    indicator: Item {}
}
