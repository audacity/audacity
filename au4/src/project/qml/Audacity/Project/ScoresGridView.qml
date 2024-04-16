/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2023 MuseScore BVBA and others
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
import MuseScore.Project 1.0

import "internal/ScoresPage"

Item {
    id: root

    property AbstractScoresModel model
    property string searchText

    property color backgroundColor: ui.theme.backgroundSecondaryColor
    property real sideMargin: 46

    property alias view: view

    property alias navigation: navPanel

    signal createNewScoreRequested()
    signal openScoreRequested(var scorePath, var displayName)

    clip: true

    SortFilterProxyModel {
        id: searchFilterModel
        sourceModel: root.model

        alwaysIncludeIndices: root.model.nonScoreItemIndices

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
                enabled: !Boolean(root.searchText) || searchFilterModel.rowCount > root.model.nonScoreItemIndices.length
                async: true
            }
        ]
    }

    NavigationPanel {
        id: navPanel
        name: "ScoresGridView"
        direction: NavigationPanel.Both
        accessible.name: qsTrc("project", "Scores grid")
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

        readonly property real spacingBetweenColumns: 60
        readonly property real spacingBetweenRows: 40

        readonly property real actualCellWidth: 172
        readonly property real actualCellHeight: 294

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

            ScoreGridItem {
                anchors.centerIn: parent

                width: view.actualCellWidth
                height: view.actualCellHeight

                navigation.panel: navPanel
                navigation.row: view.columns === 0 ? 0 : Math.floor(model.index / view.columns)
                navigation.column: (model.index - (navigation.row * view.columns)) * 3 // * 3 because of controls inside ScoreItem
                navigation.onActiveChanged: {
                    if (navigation.active) {
                        view.positionViewAtIndex(index, GridView.Contain)
                    }
                }

                name: score.name
                path: score.path ?? ""
                suffix: score.suffix ?? ""
                thumbnailUrl: score.thumbnailUrl ?? ""
                isCreateNew: score.isCreateNew
                isNoResultsFound: score.isNoResultsFound
                isCloud: score.isCloud
                cloudScoreId: score.scoreId ?? 0
                timeSinceModified: score.timeSinceModified ?? ""

                onClicked: {
                    if (isCreateNew) {
                        root.createNewScoreRequested()
                    } else if (!isNoResultsFound) {
                        root.openScoreRequested(score.path, score.name)
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
