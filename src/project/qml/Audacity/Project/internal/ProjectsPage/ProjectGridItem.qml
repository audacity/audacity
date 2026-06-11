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
import QtQuick.Layouts 1.15
import QtQuick.Window 2.15

import Muse.Ui 1.0
import Muse.UiComponents
import Muse.GraphicalEffects 1.0
import Audacity.Project 1.0

FocusScope {
    id: root

    property string name: ""
    property string path: ""
    property string thumbnailUrl: ""
    property alias timeSinceModified: timeSinceModified.text
    property string placeholder: ""
    property bool isCreateNew: false
    property bool isNoResultsFound: false

    property var contextMenuModel: null
    property bool showIndicator: false
    property Component indicatorButton: null

    property alias navigation: navCtrl

    signal clicked

    Component.onCompleted: {
        if (root.contextMenuModel != null) {
            root.contextMenuModel.load()
        }
    }

    onContextMenuModelChanged: {
        if (root.contextMenuModel != null) {
            root.contextMenuModel.load()
        }
    }

    NavigationControl {
        id: navCtrl
        name: root.name
        enabled: root.enabled && root.visible

        accessible.role: MUAccessible.Button
        accessible.name: root.name

        onActiveChanged: function (active) {
            if (active) {
                root.forceActiveFocus()
            }
        }

        onTriggered: root.clicked()
    }

    MouseArea {
        id: mouseArea

        anchors.fill: parent

        hoverEnabled: true

        onClicked: {
            root.clicked()
        }
    }

    Column {
        anchors.fill: parent

        spacing: 16

        Item {
            height: 126
            width: 224

            Item {
                id: thumbnail
                anchors.fill: parent

                clip: true

                property int borderWidth: 0
                property color borderColor: ui.theme.strokeColor
                readonly property int radius: 3

                layer.enabled: true
                layer.smooth: true
                layer.textureSize: Qt.size(thumbnail.width * Screen.devicePixelRatio,
                                           thumbnail.height * Screen.devicePixelRatio)
                layer.effect: RoundedCornersEffect {
                    radius: thumbnail.radius
                }

                HoverHandler {
                    id: hoverHandler
                }

                Loader {
                    id: loader

                    anchors.fill: parent

                    sourceComponent: {
                        if (root.isCreateNew) {
                            return addComp
                        }

                        if (root.isNoResultsFound) {
                            return noResultFoundComp
                        }

                        return projectItemComp
                    }
                }

                Rectangle {
                    anchors.fill: parent

                    color: "transparent"
                    radius: parent.radius

                    NavigationFocusBorder {
                        navigationCtrl: navCtrl

                        padding: 2
                    }

                    border.color: parent.borderColor
                    border.width: parent.borderWidth
                }

                states: [
                    State {
                        name: "NORMAL"
                        when: !hoverHandler.hovered && !mouseArea.pressed

                        PropertyChanges {
                            target: thumbnail
                            borderWidth: 1
                        }
                    },
                    State {
                        name: "HOVERED"
                        when: hoverHandler.hovered && !mouseArea.pressed

                        PropertyChanges {
                            target: thumbnail
                            borderWidth: 1
                            borderColor: ui.theme.accentColor
                        }
                    },
                    State {
                        name: "PRESSED"
                        when: mouseArea.pressed

                        PropertyChanges {
                            target: thumbnail
                            borderWidth: 1
                            borderColor: ui.theme.strokeColor
                        }
                    }
                ]

                EffectRectangularGlow {
                    anchors.fill: thumbnail
                    z: -1

                    glowRadius: 20
                    color: "#08000000"
                    cornerRadius: thumbnail.radius + glowRadius
                }
            }

            Loader {
                active: (root.contextMenuModel != null)

                anchors.top: parent.top
                anchors.topMargin: 8
                anchors.right: parent.right
                anchors.rightMargin: 8

                sourceComponent: MenuButton {
                    id: menuButton

                    width: 20
                    height: width

                    normalColor: ui.theme.buttonColor

                    menuModel: root.contextMenuModel

                    onHandleMenuItem: function (itemId) {
                        Qt.callLater(root.contextMenuModel.handleMenuItem, itemId)
                    }

                    NavigationFocusBorder {
                        navigationCtrl: NavigationControl {
                            name: "MenuButton"
                            panel: navCtrl.panel
                            row: navCtrl.row
                            column: navCtrl.column + 1
                            enabled: menuButton.visible && menuButton.enabled
                            accessible.name: qsTrc("project", "Project item menu")
                            accessible.role: MUAccessible.Button

                            onTriggered: {
                                menuButton.clicked(null)
                            }
                        }
                    }
                }
            }

            Loader {
                active: root.showIndicator && root.indicatorButton != null

                anchors.right: parent.right
                anchors.rightMargin: 8
                anchors.bottom: parent.bottom
                anchors.bottomMargin: 8

                sourceComponent: root.indicatorButton
            }
        }

        Column {
            anchors.left: parent.left
            anchors.right: parent.right

            spacing: 4

            StyledTextLabel {
                anchors.horizontalCenter: parent.horizontalCenter

                text: root.name

                wrapMode: Text.WrapAnywhere
                maximumLineCount: 1
                width: parent.width

                font: ui.theme.largeBodyFont
            }

            StyledTextLabel {
                id: timeSinceModified

                anchors.horizontalCenter: parent.horizontalCenter

                font.capitalization: Font.AllUppercase

                visible: !root.isCreateNew && !root.isNoResultsFound
            }
        }
    }

    Component {
        id: addComp

        Rectangle {
            anchors.fill: parent
            color: ui.theme.backgroundSecondaryColor

            StyledIconLabel {
                anchors.centerIn: parent

                iconCode: IconCode.PLUS

                font.pixelSize: 50
                color: ui.theme.fontPrimaryColor
            }
        }
    }

    Component {
        id: noResultFoundComp

        Rectangle {
            anchors.fill: parent
            color: ui.theme.backgroundPrimaryColor

            StyledTextLabel {
                anchors.fill: parent
                text: qsTrc("global", "No results found")
            }
        }
    }

    Component {
        id: projectItemComp

        ProjectThumbnail {
            path: root.thumbnailUrl
            placeholder: root.placeholder

            backgroundColor: ui.theme.backgroundSecondaryColor
            lineColor: Qt.alpha(ui.theme.fontPrimaryColor, 0.8)
            borderColor: "transparent"
        }
    }
}
