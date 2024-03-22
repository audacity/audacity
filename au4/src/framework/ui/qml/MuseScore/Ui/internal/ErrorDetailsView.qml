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

Column {
    id: root

    property string detailedText: ""

    property NavigationSection navigationSection: null
    property int navigationOrder: 0

    spacing: 16

    ErrorDetailsModel {
        id: detailsModel
    }

    Component.onCompleted: {
        detailsModel.load(root.detailedText)
    }

    Rectangle {
        height: 120
        width: parent.width

        radius: 3

        border.width: 1
        border.color: ui.theme.strokeColor

        color: "transparent"

        StyledListView {
            id: detailsView

            anchors.fill: parent
            anchors.margins: 1

            contentWidth: contentItem.childrenRect.width
            flickableDirection: Flickable.AutoFlickDirection

            spacing: 0

            model: detailsModel

            NavigationPanel {
                id: detailsViewNavPanel

                name: "DetailsViewNavPanel"
                order: root.navigationOrder
                enabled: root.enabled && root.visible
                direction: NavigationPanel.Horizontal
                section: root.navigationSection
            }

            delegate: ListItemBlank {
                navigation.name: "Error " + model.index
                navigation.panel: detailsViewNavPanel
                navigation.row: model.index
                navigation.accessible.name: model.errorPlainText
                navigation.onActiveChanged: {
                    if (navigation.active) {
                        detailsView.positionViewAtIndex(index, ListView.Contain)
                    }
                }

                background.color: model.index % 2 === 0 ? ui.theme.backgroundSecondaryColor : "transparent"
                mouseArea.enabled: false

                implicitWidth: label.implicitWidth + 2 * 30
                width: Math.max(ListView.view.width, implicitWidth)

                StyledTextLabel {
                    id: label
                    anchors.fill: parent
                    anchors.leftMargin: 30
                    anchors.rightMargin: 30

                    verticalAlignment: Text.AlignVCenter
                    horizontalAlignment: Text.AlignLeft

                    width: parent.width

                    textFormat: Qt.RichText
                    text: model.errorText
                }
            }
        }
    }

    RowLayout {
        spacing: 0

        NavigationPanel {
            id: copyDetailsNavPanel

            name: "CopyDetailsNavPanel"
            order: root.navigationOrder + 1
            enabled: root.enabled && root.visible
            direction: NavigationPanel.Horizontal
            section: root.navigationSection
        }

        FlatButton {
            text: qsTrc("global", "Copy")

            navigation.name: "CopyButton"
            navigation.panel: copyDetailsNavPanel
            navigation.onActiveChanged: {
                if (!navigation.active) {
                    detailsCopiedMessage.resetFocus()
                }
            }

            onClicked: {
                detailsCopiedMessage.visible = detailsModel.copyDetailsToClipboard()

                if (detailsCopiedMessage.visible) {
                    detailsCopiedMessage.readInfo()
                }
            }
        }

        StyledIconLabel {
            Layout.leftMargin: 12
            Layout.alignment: Qt.AlignVCenter

            iconCode: IconCode.TICK_RIGHT_ANGLE

            visible: detailsCopiedMessage.visible
        }

        StyledTextLabel {
            id: detailsCopiedMessage

            Layout.leftMargin: 6
            Layout.fillWidth: true
            Layout.maximumWidth: 308
            Layout.alignment: Qt.AlignVCenter

            verticalAlignment: Text.AlignVCenter
            horizontalAlignment: Text.AlignLeft

            wrapMode: Text.WordWrap
            maximumLineCount: 2

            visible: false

            text: qsTrc("global", "Error details have been copied to the clipboard.")

            AccessibleItem {
                id: accessibleInfo

                accessibleParent: copyDetailsNavPanel.accessible
                visualItem: detailsCopiedMessage
                role: MUAccessible.StaticText
                name: detailsCopiedMessage.text
            }

            function readInfo() {
                accessibleInfo.ignored = false
                accessibleInfo.focused = true
            }

            function resetFocus() {
                accessibleInfo.ignored = true
                accessibleInfo.focused = false
            }
        }
    }
}
