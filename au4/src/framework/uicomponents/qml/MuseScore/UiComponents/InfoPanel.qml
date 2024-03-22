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

PopupPanel {
    id: root

    property string title: ""
    property var additionalInfoModel: undefined
    property string description: ""

    property Component buttonsPanel: null
    property var mainButton: null

    property NavigationPanel contentNavigation: NavigationPanel {
        name: root.objectName != "" ? root.objectName : "InfoPanel"

        enabled: root.visible
        section: root.navigationSection
        order: 1

        onActiveChanged: function(active) {
            if (active) {
                root.forceActiveFocus()
            }
        }
    }

    height: 360

    visible: false

    accessible.name: root.title

    content: Column {
        id: content
        anchors.fill: parent
        anchors.topMargin: 44
        anchors.leftMargin: 68
        anchors.rightMargin: 68
        anchors.bottomMargin: 42

        spacing: 42

        property bool opened: root.visible
        onOpenedChanged: {
            if (opened) {
                Qt.callLater(focusOnOpened)
            } else {
                Qt.callLater(resetFocusOnInfo)
            }
        }

        function focusOnOpened() {
            if (Boolean(root.mainButton)) {
                root.mainButton.navigation.requestActive()
            }

            readInfo()
        }

        function readInfo() {
            accessibleInfo.ignored = false
            accessibleInfo.focused = true
        }

        function resetFocusOnInfo() {
            accessibleInfo.ignored = true
            accessibleInfo.focused = false
        }

        AccessibleItem {
            id: accessibleInfo
            accessibleParent: root.accessible
            visualItem: root
            role: MUAccessible.Button
            name: {
                var text = root.title + "."

                if (Boolean(root.additionalInfoModel)) {
                    for (var i = 0; i < root.additionalInfoModel.length; i++) {
                        text += " " + root.additionalInfoModel[i].title + " " + root.additionalInfoModel[i].value + ". "
                    }
                }

                text += root.description + ". " + root.mainButton.text

                return text
            }
        }

        Column {
            width: 585

            spacing: 8

            StyledTextLabel {
                id: titleLabel

                text: Boolean(root.title) ? root.title : ""
                font: ui.theme.headerBoldFont
            }

            Row {
                anchors.left: parent.left
                anchors.right: parent.right

                spacing: 4

                visible: Boolean(root.additionalInfoModel)

                Repeater {
                    model: root.additionalInfoModel
                    Row {
                        spacing: 4
                        Rectangle {
                            width: 2
                            height: parent.height - 4
                            anchors.verticalCenter: parent.verticalCenter
                            color: ui.theme.fontPrimaryColor

                            visible: index !== 0
                        }

                        StyledTextLabel {
                            font: ui.theme.largeBodyFont
                            text: modelData.title
                        }

                        StyledTextLabel {
                            font: ui.theme.largeBodyBoldFont
                            text: modelData.value
                        }
                    }
                }
            }
        }

        StyledTextLabel {
            width: 585
            height: 88

            opacity: 0.75
            wrapMode: Text.WordWrap
            verticalAlignment: Text.AlignTop
            horizontalAlignment: Text.AlignLeft

            text: Boolean(root.description) ? root.description : ""
            visible: Boolean(root.description)
        }

        Loader {
            id: buttonsPanelLoader

            width: parent.width

            sourceComponent: root.buttonsPanel
        }
    }
}
