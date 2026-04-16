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

import Muse.Ui 1.0
import Muse.UiComponents
import Muse.GraphicalEffects 1.0
import Audacity.Project 1.0

ListItemBlank {
    id: root

    required property var item
    property alias columns: columnsRepeater.model

    property Component thumbnailComponent: defaultThumbnailComponent

    property real itemInset: 12
    property real columnSpacing: 44
    property alias showBottomBorder: bottomBorder.visible

    property string placeholder: ""

    property bool isCloudItem: false
    property bool thumbnailFull: false

    implicitHeight: 64

    navigation.accessible.name: root.item.name ?? ""
    navigation.onActiveChanged: {
        if (navigation.active) {
            root.scrollIntoView()
        }
    }

    focusBorder.anchors.bottomMargin: bottomBorder.visible ? bottomBorder.height : 0

    QtObject {
        id: prv

        property color backgroundColor: root.thumbnailFull ? "transparent" : ui.theme.backgroundSecondaryColor
        property color lineColor: Qt.alpha(ui.theme.fontPrimaryColor, 0.8)
        property color borderColor: root.thumbnailFull ? "transparent" : ui.theme.strokeColor
    }

    Component {
        id: defaultThumbnailComponent

        ProjectThumbnail {
            path: root.item.thumbnailUrl ?? ""
            placeholder: root.placeholder

            backgroundColor: prv.backgroundColor
            lineColor: prv.lineColor
            borderColor: prv.borderColor
        }
    }

    RowLayout {
        anchors.fill: parent
        anchors.leftMargin: root.itemInset
        anchors.rightMargin: root.itemInset

        spacing: root.columnSpacing

        RowLayout {
            id: nameLayout

            Layout.fillWidth: true

            spacing: 24

            StyledTextLabel {
                id: projectName

                Layout.preferredWidth: 200

                text: root.item.name ?? ""
                font: ui.theme.largeBodyFont
                horizontalAlignment: Text.AlignLeft
            }

            Loader {
                Layout.fillWidth: root.thumbnailFull
                Layout.fillHeight: root.thumbnailFull
                Layout.preferredWidth: root.thumbnailFull ? -1 : 71
                Layout.preferredHeight: root.thumbnailFull ? -1 : 40

                sourceComponent: root.thumbnailComponent
            }

            Item {
                visible: !root.thumbnailFull
                Layout.fillWidth: true
            }

            Loader {
                active: root.isCloudItem ?? false
                visible: active
                Layout.alignment: Qt.AlignTrailing | Qt.AlignVCenter

                sourceComponent: CloudProjectIndicatorButton {
                    id: cloudIndicator

                    isProgress: false //cloudProjectStatusWatcher.isProgress
                    isDownloadedAndUpToDate: true //cloudProjectStatusWatcher.isDownloadedAndUpToDate

                    navigation.panel: root.navigation.panel
                    navigation.row: root.navigation.row
                    navigation.column: 3
                    navigation.onActiveChanged: {
                        if (navigation.active) {
                            root.scrollIntoView()
                        }
                    }
                }
            }
        }

        Repeater {
            id: columnsRepeater

            delegate: Loader {
                Layout.preferredWidth: modelData.width(parent.width)

                // These properties are here to give the delegate access to them
                readonly property ProjectListItem listItem: root
                readonly property var item: root.item
                readonly property NavigationPanel navigationPanel: root.navigation.panel
                readonly property int navigationRow: root.navigation.row
                readonly property int navigationColumnStart: 100 * (model.index + 1)

                sourceComponent: modelData.delegate
            }
        }
    }

    SeparatorLine {
        id: bottomBorder
        anchors.bottom: parent.bottom
    }
}
