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

ProjectsView {
    id: root

    RecentProjectsModel {
        id: recentProjectsModel
    }

    Component.onCompleted: {
        recentProjectsModel.load()
    }

    sourceComponent: root.viewType === ProjectsPageModel.List ? listComp : gridComp

    Component {
        id: gridComp

        ProjectsGridView {
            anchors.fill: parent

            model: recentProjectsModel
            searchText: root.searchText

            backgroundColor: root.backgroundColor
            sideMargin: root.sideMargin

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrder
            navigation.name: "RecentProjectsGrid"
            navigation.accessible.name: qsTrc("project", "Recent projects grid")

            onCreateNewProjectRequested: {
                root.createNewProjectRequested()
            }

            onOpenProjectRequested: function(projectPath, displayName) {
                root.openProjectRequested(projectPath, displayName)
            }
        }
    }

    Component {
        id: listComp

        ProjectsListView {
            id: list

            anchors.fill: parent

            model: recentProjectsModel
            searchText: root.searchText

            backgroundColor: root.backgroundColor
            sideMargin: root.sideMargin

            showNewProjectItem: true

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrder
            navigation.name: "RecentProjectsList"
            navigation.accessible.name: qsTrc("project", "Recent projects list")

            onCreateNewProjectRequested: {
                root.createNewProjectRequested()
            }

            onOpenProjectRequested: function(projectPath, displayName) {
                root.openProjectRequested(projectPath, displayName)
            }

            columns: [
                ProjectsListView.ColumnItem {
                    id: modifiedColumn

                    //: Stands for "Last time that this project was modified".
                    //: Used as the header of this column in the scores list.
                    header: qsTrc("project", "Modified")

                    width: function (parentWidth) {
                        let parentWidthExclusingSpacing = parentWidth - list.columns.length * list.view.columnSpacing;
                        return 0.25 * parentWidthExclusingSpacing
                    }

                    delegate: StyledTextLabel {
                        id: modifiedLabel
                        text: project.timeSinceModified ?? ""

                        font.capitalization: Font.AllUppercase
                        horizontalAlignment: Text.AlignLeft

                        NavigationFocusBorder {
                            navigationCtrl: NavigationControl {
                                name: "ModifiedLabel"
                                panel: navigationPanel
                                row: navigationRow
                                column: navigationColumnStart
                                enabled: modifiedLabel.visible && modifiedLabel.enabled && !modifiedLabel.isEmpty
                                accessible.name: modifiedColumn.header + ": " + modifiedLabel.text
                                accessible.role: MUAccessible.StaticText

                                onActiveChanged: {
                                    if (active) {
                                        listItem.scrollIntoView()
                                    }
                                }
                            }

                            anchors.margins: -radius
                            radius: 2 + border.width
                        }
                    }
                },

                ProjectsListView.ColumnItem {
                    id: sizeColumn
                    header: qsTrc("global", "Size", "file size")

                    width: function (parentWidth) {
                        let parentWidthExclusingSpacing = parentWidth - list.columns.length * list.view.columnSpacing;
                        return 0.15 * parentWidthExclusingSpacing
                    }

                    delegate: StyledTextLabel {
                        id: sizeLabel
                        text: Boolean(project.fileSize) ? project.fileSize : "-"

                        font: ui.theme.largeBodyFont
                        horizontalAlignment: Text.AlignLeft

                        NavigationFocusBorder {
                            navigationCtrl: NavigationControl {
                                name: "SizeLabel"
                                panel: navigationPanel
                                row: navigationRow
                                column: navigationColumnStart
                                enabled: sizeLabel.visible && sizeLabel.enabled && !sizeLabel.isEmpty
                                accessible.name: sizeColumn.header + ": " + (Boolean(project.fileSize) ? project.fileSize : qsTrc("global", "Unknown"))
                                accessible.role: MUAccessible.StaticText

                                onActiveChanged: {
                                    if (active) {
                                        listItem.scrollIntoView()
                                    }
                                }
                            }

                            anchors.margins: -radius
                            radius: 2 + border.width
                        }
                    }
                }
            ]
        }
    }
}
