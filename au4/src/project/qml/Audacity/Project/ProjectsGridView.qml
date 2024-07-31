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

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Audacity.Project 1.0

import "internal/ProjectsPage"

Item {
    id: root

    property AbstractProjectsModel model
    property string searchText

    property color backgroundColor: ui.theme.backgroundSecondaryColor
    property real sideMargin: 46

    property alias view: view

    property alias navigation: navPanel

    signal createNewProjectRequested()
    signal openProjectRequested(var projectPath, var displayName)

    clip: true

    SortFilterProxyModel {
        id: searchFilterModel
        sourceModel: root.model

        alwaysIncludeIndices: root.model.nonProjectItemIndices

        filters: [
            FilterValue {
                roleName: "name"
                roleValue: root.searchText
                compareType: CompareType.Contains
            }
        ]
    }

    SortFilterProxyModel {
        id: itemTypeFilterModel
        sourceModel: searchFilterModel

        filters: [
            FilterValue {
                roleName: "isNoResultsFound"
                roleValue: true
                compareType: CompareType.NotEqual
                enabled: !Boolean(root.searchText) || searchFilterModel.rowCount > root.model.nonProjectItemIndices.length
                async: true
            }
        ]
    }

    NavigationPanel {
        id: navPanel
        name: "ProjectsGridView"
        direction: NavigationPanel.Both
        accessible.name: qsTrc("project", "Projects grid")
    }

    GradientRectangle {
        id: topGradient

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top

        startColor: root.backgroundColor
        endColor: "transparent"
    }

    StyledGridView {
        id: view

        anchors.fill: parent
        anchors.topMargin: -spacingBetweenRows / 2
        anchors.leftMargin: root.sideMargin - spacingBetweenColumns / 2
        anchors.rightMargin: root.sideMargin - spacingBetweenColumns / 2
        anchors.bottomMargin: -spacingBetweenRows / 2

        topMargin: topGradient.height
        bottomMargin: bottomGradient.height

        readonly property int columns: Math.max(0, Math.floor(width / cellWidth))
        readonly property int rows: Math.max(0, Math.ceil(height / cellHeight))

        readonly property real spacingBetweenColumns: 53
        readonly property real spacingBetweenRows: 40

        readonly property real actualCellWidth: 224
        readonly property real actualCellHeight: 196

        cellWidth: actualCellWidth + spacingBetweenColumns
        cellHeight: actualCellHeight + spacingBetweenRows

        flickableDirection: Flickable.VerticalFlick

        ScrollBar.vertical: StyledScrollBar {
            parent: root

            anchors.top: parent.top
            anchors.bottom: parent.bottom
            anchors.right: parent.right

            visible: view.contentHeight > view.height
            z: 2
        }

        model: itemTypeFilterModel

        delegate: Item {
            width: view.cellWidth
            height: view.cellHeight

            ProjectGridItem {
                anchors.centerIn: parent

                width: view.actualCellWidth
                height: view.actualCellHeight

                navigation.panel: navPanel
                navigation.row: view.columns === 0 ? 0 : Math.floor(model.index / view.columns)
                navigation.column: (model.index - (navigation.row * view.columns)) * 3 // * 3 because of controls inside ProjectItem
                navigation.onActiveChanged: {
                    if (navigation.active) {
                        view.positionViewAtIndex(index, GridView.Contain)
                    }
                }

                name: project.name
                path: project.path ?? ""
                suffix: project.suffix ?? ""
                thumbnailUrl: Qt.resolvedUrl("file:" + project.thumbnailUrl) ?? ""
                isCreateNew: project.isCreateNew
                isNoResultsFound: project.isNoResultsFound
                isCloud: project.isCloud
                cloudProjectId: project.projectId ?? 0
                timeSinceModified: project.timeSinceModified ?? ""

                onClicked: {
                    if (isCreateNew) {
                        root.createNewProjectRequested()
                    } else if (!isNoResultsFound) {
                        root.openProjectRequested(project.path, project.name)
                    }
                }
            }
        }
    }

    GradientRectangle {
        id: bottomGradient
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom

        startColor: "transparent"
        endColor: root.backgroundColor
    }

    Item {
        id: noResultsMessage
        anchors.fill: parent

        // This will become visible if a "No results found" item is not provided by the model.
        visible: Boolean(root.searchText) && itemTypeFilterModel.rowCount === 0

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
