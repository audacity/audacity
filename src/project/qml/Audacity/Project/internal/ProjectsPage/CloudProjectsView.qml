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

    function refresh() {
        cloudProjectsModel.reload()
    }

    CloudProjectsModel {
        id: cloudProjectsModel

        onStateChanged: {
            if (cloudProjectsModel.state === CloudProjectsModel.Fine) {
                prv.updateDesiredRowCount()
            }
        }
    }

    Component.onCompleted: {
        cloudProjectsModel.load()
    }

    Connections {
        target: root.item ? root.item.view : null
        
        function onContentYChanged() {
            prv.updateDesiredRowCount()
        }
    }

    QtObject {
        id: prv
        property bool updateDesiredRowCountScheduled: false
        
        readonly property var activeView: root.item
        
        readonly property int remainingFullRowsBelowViewport: {
            if (!activeView || !activeView.view) {
                return 0
            }
            
            let view = activeView.view
            let columns = view.columns || 1
            let cellHeight = view.cellHeight || 100
            let topMargin = view.topMargin || 0

            let totalDataRows = Math.ceil(cloudProjectsModel.rowCount / columns)
            let scrolledContent = view.contentY + topMargin
            let currentScrollRow = Math.max(0, Math.floor(scrolledContent / cellHeight))
            let visibleRows = Math.ceil((view.height + (scrolledContent % cellHeight)) / cellHeight)
            let viewportBottomRow = currentScrollRow + visibleRows
 
            return Math.max(0, totalDataRows - viewportBottomRow)
        }
        
        readonly property bool isSatisfied: remainingFullRowsBelowViewport >= 2
        
        onIsSatisfiedChanged: {
            if (!isSatisfied) {
                updateDesiredRowCount()
            }
        }
        
        function updateDesiredRowCount() {
            if (updateDesiredRowCountScheduled) {
                return
            }
            
            if (isSatisfied || !cloudProjectsModel.hasMore) {
                return
            }
            
            updateDesiredRowCountScheduled = true
            
            Qt.callLater(function() {
                let view = activeView ? activeView.view : null
                let columns = view ? (view.columns || 1) : 1
                
                let rowsToAdd = Math.max(3 - remainingFullRowsBelowViewport, 1)
                let newDesiredRowCount = cloudProjectsModel.rowCount + rowsToAdd * columns
                
                if (cloudProjectsModel.desiredRowCount < newDesiredRowCount) {
                    cloudProjectsModel.desiredRowCount = newDesiredRowCount
                }
                
                updateDesiredRowCountScheduled = false
            })
        }
    }

    sourceComponent: root.viewType === ProjectsPageModel.List ? listComp : gridComp

    Component {
        id: gridComp

        ProjectsGridView {
            id: gridView

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
