/*
* Audacity: A Digital Audio Editor
*/

import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.Ui 1.0
import Muse.UiComponents
import Audacity.Project 1.0

ProjectsView {
    id: root

    CloudProjectsModel {
        id: cloudProjectsModel
    }

    Component.onCompleted: {
        cloudProjectsModel.load()
    }

    sourceComponent: root.viewType === ProjectsPageModel.List ? listComp : gridComp

    Component {
        id: gridComp

        ProjectsGridView {
            anchors.fill: parent

            model: cloudProjectsModel
            searchText: root.searchText

            backgroundColor: root.backgroundColor
            sideMargin: root.sideMargin

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrder
            navigation.name: "CloudProjectsGrid"
            navigation.accessible.name: qsTrc("project", "Cloud projects grid")

            //onCreateNewProjectRequested: {}
            //onOpenProjectRequested: function(projectPath, displayName) {}
        }
    }

    Component {
        id: listComp

        ProjectsListView {
            id: list

            anchors.fill: parent

            model: cloudProjectsModel
            searchText: root.searchText

            backgroundColor: root.backgroundColor
            sideMargin: root.sideMargin

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrder
            navigation.name: "CloudProjectsList"
            navigation.accessible.name: qsTrc("project", "Cloud projects list")

            //onCreateNewProjectRequested: {}
            //onOpenProjectRequested: function(projectPath, displayName) {}

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