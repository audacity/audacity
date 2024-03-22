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
import MuseScore.UiComponents 1.0

RadioDelegate {
    id: root

    property Component iconComponent: null
    property string title: ""
    property bool iconOnly: false

    property int orientation: Qt.Vertical
    readonly property bool isVertical: orientation === Qt.Vertical

    property var normalStateFont: ui.theme.largeBodyFont
    property var selectedStateFont: ui.theme.largeBodyBoldFont

    property alias navigation: navCtrl

    height: isVertical ? 36 : 44

    spacing: 30
    leftPadding: 0
    rightPadding: 0

    onToggled: {
        navigation.requestActiveByInteraction()
    }

    NavigationControl {
        id: navCtrl
        name: root.objectName
        enabled: root.enabled && root.visible

        accessible.role: MUAccessible.RadioButton
        accessible.name: root.title
        accessible.checked: root.checked

        onActiveChanged: {
            if (navCtrl.active) {
                root.forceActiveFocus()
            }
        }

        onTriggered: root.toggled()
    }

    background: Rectangle {
        id: backgroundRect
        anchors.fill: parent

        color: ui.theme.backgroundPrimaryColor
        opacity: ui.theme.buttonOpacityNormal

        border.color: navCtrl.highlight ? ui.theme.fontPrimaryColor : ui.theme.strokeColor
        border.width: navCtrl.highlight ? ui.theme.navCtrlBorderWidth : ui.theme.borderWidth

        Rectangle {
            id: line
            color: ui.theme.accentColor
            visible: false
            anchors.margins: backgroundRect.border.width

            states: [
                State {
                    name: "vertical"
                    when: root.isVertical

                    AnchorChanges {
                        target: line
                        anchors.left: parent.left
                        anchors.right: parent.right
                        anchors.bottom: parent.bottom
                    }

                    PropertyChanges {
                        target: line
                        height: 2
                    }
                },

                State {
                    name: "horizontal"
                    when: !root.isVertical

                    AnchorChanges {
                        target: line
                        anchors.top: parent.top
                        anchors.left: parent.left
                        anchors.bottom: parent.bottom
                    }

                    PropertyChanges {
                        target: line
                        width: 2
                    }
                }
            ]
        }
    }

    contentItem: Item {
        anchors.fill: parent
        anchors.leftMargin: root.leftPadding
        anchors.rightMargin: root.rightPadding
        anchors.bottomMargin: root.isVertical ? 2 : 0

        implicitWidth: root.leftPadding + contentRow.implicitWidth + root.rightPadding
        implicitHeight: contentRow.implicitHeight

        Row {
            id: contentRow
            anchors.fill: parent
            spacing: root.spacing

            Loader {
                anchors.verticalCenter: parent.verticalCenter
                sourceComponent: root.iconComponent
                visible: Boolean(root.iconComponent)
            }

            StyledTextLabel {
                id: textLabel
                anchors.verticalCenter: parent.verticalCenter

                horizontalAlignment: Text.AlignLeft
                font: root.normalStateFont
                text: root.title

                visible: !root.iconOnly
            }
        }
    }

    indicator: Item {}

    states: [
        State {
            name: "HOVERED"
            when: root.hovered && !root.checked && !root.pressed

            PropertyChanges {
                target: backgroundRect
                color: ui.theme.buttonColor
                opacity: ui.theme.buttonOpacityHover
            }
        },

        State {
            name: "PRESSED"
            when: root.pressed && !root.checked

            PropertyChanges {
                target: backgroundRect
                color: ui.theme.buttonColor
                opacity: ui.theme.buttonOpacityHit
            }
        },

        State {
            name: "SELECTED"
            when: root.checked

            PropertyChanges {
                target: backgroundRect
                color: Utils.colorWithAlpha(ui.theme.accentColor, 0.1)
                opacity: 1.0
            }

            PropertyChanges {
                target: line
                visible: true
            }

            PropertyChanges {
                target: textLabel
                font: root.selectedStateFont
            }
        }
    ]
}
