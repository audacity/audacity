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
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Audacity.Project 1.0

import "internal/ProjectsPage"

Item {
    id: root

    property AbstractProjectsModel model
    property list<ColumnItem> columns
    property alias showNewProjectItem: newProjectItem.visible
    property string searchText

    property color backgroundColor: ui.theme.backgroundSecondaryColor
    property real sideMargin: 46

    property alias view: view

    property alias navigation: navPanel

    signal createNewProjectRequested()
    signal openProjectRequested(var projectPath, var displayName)

    component ColumnItem : QtObject {
        property string header

        property var width: function (parentWidth) {
            return parentWidth / 5
        }

        property Component delegate
    }

    SortFilterProxyModel {
        id: searchFilterModel
        sourceModel: root.model

        alwaysExcludeIndices: root.model.nonProjectItemIndices

        filters: [
            FilterValue {
                roleName: "name"
                roleValue: root.searchText
                compareType: CompareType.Contains
            }
        ]
    }

    NavigationPanel {
        id: navPanel
        name: "ProjectsListView"
        direction: NavigationPanel.Both
        accessible.name: qsTrc("project", "Projects list")
    }

    ColumnLayout {
        anchors.fill: parent
        anchors.leftMargin: sideMargin
        anchors.rightMargin: sideMargin

        spacing: 12

        ProjectListItem {
            id: newProjectItem

            Layout.fillWidth: true
            Layout.alignment: Qt.AlignTop | Qt.AlignHCenter
            implicitHeight: view.rowHeight

            visible: false
            itemInset: view.itemInset
            showBottomBorder: false

            navigation.panel: navPanel
            navigation.row: 0
            navigation.column: 0

            project: {
                "name": qsTrc("project", "New project")
            }

            thumbnailComponent: Rectangle {
                anchors.fill: parent
                color: "white"

                StyledIconLabel {
                    anchors.centerIn: parent

                    iconCode: IconCode.PLUS

                    font.pixelSize: 16
                    color: "black"
                }
            }

            onClicked: root.createNewProjectRequested()
        }

        Item {
            id: listViewContainer

            Layout.fillWidth: true
            Layout.fillHeight: true

            visible: view.count > 0 || view.header || view.footer

            ColumnLayout {
                id: listViewColumn

                anchors.fill: parent
                spacing: 0

                // Column headers
                RowLayout {
                    Layout.preferredHeight: 44
                    Layout.leftMargin: view.itemInset
                    Layout.rightMargin: view.itemInset

                    spacing: view.columnSpacing

                    StyledTextLabel {
                        Layout.fillWidth: true

                        text: qsTrc("project", "Name")

                        // It is not possible to set the `font` and `font.capitalization` properties at the same time.
                        // The following alternatives do not work:
                        // - font: { let f = ui.theme.bodyBoldFont; f.capitalization = Font.AllUppercase; return f }
                        //
                        // - font: ui.theme.bodyBoldFont
                        //   Component.onCompleted: { font.capitalization = Font.AllUppercase }
                        //   (breaks updating the font when changed in Preferences > Appearance
                        //
                        // - Qt.font(Object.assign(ui.theme.bodyBoldFont, { capitalization: Font.AllUppercase }))
                        //   (complains that ui.theme.bodyBoldFont is const and cannot be modified)
                        font: Qt.font(Object.assign({}, ui.theme.bodyBoldFont, { capitalization: Font.AllUppercase }))
                        horizontalAlignment: Text.AlignLeft
                    }

                    Repeater {
                        model: root.columns

                        delegate: StyledTextLabel {
                            Layout.preferredWidth: modelData.width(parent.width)

                            text: modelData.header

                            font: Qt.font(Object.assign({}, ui.theme.bodyBoldFont, { capitalization: Font.AllUppercase }))
                            horizontalAlignment: Text.AlignLeft
                        }
                    }
                }

                SeparatorLine {}

                StyledListView {
                    id: view

                    Layout.fillWidth: true
                    Layout.fillHeight: true

                    bottomMargin: bottomGradient.height

                    readonly property real itemInset: 12
                    readonly property real rowHeight: 64
                    readonly property real columnSpacing: 44

                    ScrollBar.vertical: StyledScrollBar {
                        parent: root

                        anchors.top: parent.top
                        anchors.bottom: parent.bottom
                        anchors.right: parent.right

                        visible: view.contentHeight > view.height
                        z: 2
                    }

                    model: searchFilterModel

                    delegate: ProjectListItem {
                        required property int index

                        columns: root.columns

                        itemInset: view.itemInset
                        implicitHeight: view.rowHeight
                        columnSpacing: view.columnSpacing

                        navigation.panel: navPanel
                        navigation.row: index + 1
                        navigation.column: 0

                        onClicked: {
                            root.openProjectRequested(project.path, project.name)
                        }
                    }
                }
            }

            Rectangle {
                id: bottomGradient
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.bottom: parent.bottom

                visible: listViewColumn.visible

                height: 8
                z: 1

                gradient: Gradient {
                    GradientStop {
                        position: 0.0
                        color: "transparent"
                    }

                    GradientStop {
                        position: 1.0
                        color: root.backgroundColor
                    }
                }
            }
        }

        Item {
            id: noResultsMessage

            Layout.fillWidth: true
            Layout.fillHeight: true

            visible: Boolean(root.searchText) && !listViewContainer.visible

            Message {
                anchors.top: parent.top
                anchors.topMargin: Math.max(parent.height / 3 - height / 2, 0)
                anchors.left: parent.left
                anchors.leftMargin: root.sideMargin
                anchors.right: parent.right
                anchors.rightMargin: root.sideMargin

                title: qsTrc("global", "No results found")
            }
        }
    }
}
