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
import MuseScore.UiComponents 1.0

FocusScope {
    id: root

    property int icon: IconCode.NONE
    property string text: ""
    property int textFormat: Text.AutoText

    //!NOTE: used to sort buttons inside a button box
    property int buttonId: 0
    property int buttonRole: 0
    property bool isLeftSide: false

    property string toolTipTitle: ""
    property string toolTipDescription: ""
    property string toolTipShortcut: ""

    property font iconFont: ui.theme.iconsFont
    property font textFont: ui.theme.bodyFont

    property bool transparent: false
    property bool accentButton: false

    property color normalColor:
        transparent ? "transparent" : accentButton ? accentColor : ui.theme.buttonColor
    property color hoverHitColor: accentButton ? accentColor : ui.theme.buttonColor
    property color accentColor: ui.theme.accentColor

    property bool isNarrow: buttonType === FlatButton.Horizontal
    property real margins: isNarrow ? 12 : 16
    property real minWidth: isNarrow ? 24 : 132

    property bool drawFocusBorderInsideRect: false

    property int orientation: Qt.Vertical
    readonly property bool isVertical: root.orientation === Qt.Vertical

    property alias navigation: navCtrl
    property alias accessible: navCtrl.accessible

    property alias mouseArea: mouseArea

    property bool isClickOnKeyNavTriggered: true

    property Component contentItem: null
    property Component backgroundItem: defaultBackgroundComponent

    enum ButtonType {
        TextOnly,
        IconOnly,
        Horizontal,
        Vertical,
        Custom
    }

    // Can be overridden, for buttons that have a custom content component
    // but should be sized as one of the default types
    property int buttonType: {
        if (contentItem) {
            return FlatButton.Custom
        }

        if (icon !== IconCode.NONE) {
            if (Boolean(text)) {
                return isVertical ? FlatButton.Vertical : FlatButton.Horizontal
            }

            return FlatButton.IconOnly
        }

        return FlatButton.TextOnly
    }

    signal clicked(var mouse)
    // There are intentionally no "forwarded" signals here from the MouseArea, like `pressAndHold`
    // See https://github.com/musescore/MuseScore/issues/16012#issuecomment-1399656043

    objectName: root.text

    implicitWidth: contentLoader.itemImplicitWidth + 2 * margins
    implicitHeight: Math.max(contentLoader.itemImplicitHeight, ui.theme.defaultButtonSize)

    opacity: root.enabled ? 1.0 : ui.theme.itemOpacityDisabled

    NavigationControl {
        id: navCtrl
        name: root.objectName !== "" ? root.objectName : "FlatButton"
        enabled: root.enabled && root.visible

        accessible.role: MUAccessible.Button
        accessible.name: Boolean(root.text) ? root.text : root.toolTipTitle
        accessible.description: root.toolTipDescription
        accessible.visualItem: root
        accessible.enabled: navCtrl.enabled

        onTriggered: {
            if (navCtrl.enabled && root.isClickOnKeyNavTriggered) {
                root.clicked(null)
            }
        }
    }

    Loader {
        anchors.fill: parent

        sourceComponent: root.backgroundItem
    }

    Component {
        id: defaultBackgroundComponent

        Rectangle {
            id: background

            color: root.normalColor
            opacity: ui.theme.buttonOpacityNormal

            radius: 3
            border.width: ui.theme.borderWidth
            border.color: ui.theme.strokeColor

            NavigationFocusBorder {
                navigationCtrl: navCtrl
                drawOutsideParent: !root.drawFocusBorderInsideRect
            }

            states: [
                State {
                    name: "PRESSED"
                    when: mouseArea.pressed

                    PropertyChanges {
                        target: background
                        color: root.hoverHitColor
                        opacity: ui.theme.buttonOpacityHit
                    }
                },

                State {
                    name: "HOVERED"
                    when: mouseArea.containsMouse && !mouseArea.pressed

                    PropertyChanges {
                        target: background
                        color: root.hoverHitColor
                        opacity: ui.theme.buttonOpacityHover
                    }
                }
            ]
        }
    }

    Loader {
        id: contentLoader

        anchors.verticalCenter: parent ? parent.verticalCenter : undefined
        anchors.horizontalCenter: parent ? parent.horizontalCenter : undefined

        readonly property real itemImplicitWidth: item ? item.implicitWidth : 0
        readonly property real itemImplicitHeight: item ? item.implicitHeight : 0

        sourceComponent: root.contentItem ? root.contentItem : defaultContentComponent
        readonly property Component defaultContentComponent: root.isVertical ? verticalContentComponent : horizontalContentComponent
    }

    Component {
        id: verticalContentComponent

        ColumnLayout {
            width: Math.min(implicitWidth, root.width)
            spacing: 4

            StyledIconLabel {
                Layout.alignment: Qt.AlignHCenter
                iconCode: root.icon
                font: root.iconFont
                visible: !isEmpty
            }

            StyledTextLabel {
                Layout.fillWidth: true
                Layout.alignment: Qt.AlignHCenter
                text: root.text
                font: root.textFont
                textFormat: root.textFormat
                maximumLineCount: 1
                visible: !isEmpty
            }
        }
    }

    Component {
        id: horizontalContentComponent

        RowLayout {
            width: Math.min(implicitWidth, root.width)
            spacing: 8

            StyledIconLabel {
                Layout.alignment: Qt.AlignVCenter
                iconCode: root.icon
                font: root.iconFont
                visible: !isEmpty
            }

            StyledTextLabel {
                Layout.fillWidth: true
                Layout.alignment: Qt.AlignVCenter
                text: root.text
                font: root.textFont
                textFormat: root.textFormat
                maximumLineCount: 1
                visible: !isEmpty
            }
        }
    }

    states: [
        State {
            name: "ICON_ONLY"
            when: root.buttonType === FlatButton.IconOnly

            PropertyChanges {
                target: root
                implicitWidth: ui.theme.defaultButtonSize
                implicitHeight: ui.theme.defaultButtonSize
            }
        },

        State {
            name: "TEXT_ONLY"
            when: root.buttonType === FlatButton.TextOnly

            PropertyChanges {
                target: root
                implicitWidth: Math.max(contentLoader.itemImplicitWidth + 2 * root.margins,
                                        root.minWidth)
                implicitHeight: ui.theme.defaultButtonSize
            }
        },

        State {
            name: "HORIZONTAL"
            when: root.buttonType === FlatButton.Horizontal

            PropertyChanges {
                target: root
                implicitHeight: ui.theme.defaultButtonSize
            }
        },

        State {
            name: "VERTICAL"
            when: root.buttonType === FlatButton.Vertical

            PropertyChanges {
                target: root
                implicitHeight: 48
            }

        }
    ]

    MouseArea {
        id: mouseArea
        anchors.fill: parent

        hoverEnabled: true

        onClicked: function(mouse) {
            navigation.requestActiveByInteraction()

            root.clicked(mouse)
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
