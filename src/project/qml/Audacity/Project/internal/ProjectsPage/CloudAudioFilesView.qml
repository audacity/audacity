/*
* Audacity: A Digital Audio Editor
*/

import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.Ui 1.0
import Muse.UiComponents
import Audacity.Project 1.0

import "../../."

ProjectsView {
    id: root

    function refresh() {
        cloudAudioFilesModel.reload()
        prv.updateDesiredRowCount()
    }

    CloudAudioFilesModel {
        id: cloudAudioFilesModel
        
        onStateChanged: {
            if (cloudAudioFilesModel.state === CloudAudioFilesModel.Fine) {
                prv.updateDesiredRowCount()
            }
        }
    }

    Component.onCompleted: {
        cloudAudioFilesModel.load()
    }
 
    Connections {
        target: root.item ? root.item.view : null
        
        function onContentYChanged() {
            prv.updateDesiredRowCount()
        }
    }

    QtObject {
        id: prv
        property string gridPlaceholderFile: "qrc:/resources/AudioFilePlaceholder.svg"
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

            let totalDataRows = Math.ceil(cloudAudioFilesModel.rowCount / columns)
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
            
            if (isSatisfied || !cloudAudioFilesModel.hasMore) {
                return
            }
            
            updateDesiredRowCountScheduled = true
            
            Qt.callLater(function() {
                let view = activeView ? activeView.view : null
                let columns = view ? (view.columns || 1) : 1
                
                let rowsToAdd = Math.max(3 - remainingFullRowsBelowViewport, 1)
                let newDesiredRowCount = cloudAudioFilesModel.rowCount + rowsToAdd * columns
                
                if (cloudAudioFilesModel.desiredRowCount < newDesiredRowCount) {
                    cloudAudioFilesModel.desiredRowCount = newDesiredRowCount
                }
                
                updateDesiredRowCountScheduled = false
            })
        }
    }

    sourceComponent: {
        switch(cloudAudioFilesModel.state) {
            case CloudAudioFilesModel.NotSignedIn:
                return notSignedInComp
            case CloudAudioFilesModel.Error:
                return errorComp
            case CloudAudioFilesModel.Fine:
            case CloudAudioFilesModel.Loading:
                break;
        }

        if (cloudAudioFilesModel.rowCount == 0 && !cloudAudioFilesModel.hasMore && cloudAudioFilesModel.state != CloudAudioFilesModel.Loading) {
            return emptyComp
        }

        return root.viewType === ProjectsPageModel.List ? listComp : gridComp
    }

    Component {
        id: gridComp

        ProjectsGridView {
            id: gridView
            
            anchors.fill: parent

            model: cloudAudioFilesModel
            searchText: root.searchText

            backgroundColor: root.backgroundColor
            sideMargin: root.sideMargin
            placeholder: prv.gridPlaceholderFile

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrder
            navigation.name: "CloudAudioFilesGrid"
            navigation.accessible.name: qsTrc("project", "Cloud audio files grid")

            //onCreateNewProjectRequested: {}
            //onOpenProjectRequested: function(projectPath, displayName) {}
        }
    }

    Component {
        id: listComp

        AudioListView {
            id: listView

            anchors.fill: parent

            model: cloudAudioFilesModel
            searchText: root.searchText

            backgroundColor: root.backgroundColor
            sideMargin: root.sideMargin

            itemNormalColor: ui.theme.extra["white_color"]
            itemHoverHitColor: ui.theme.buttonColor
            itemSpacing: 16

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrder
            navigation.name: "CloudAudioFilesList"
            navigation.accessible.name: qsTrc("project", "Cloud audio files list")

            //onCreateNewProjectRequested: {}
            //onOpenProjectRequested: function(projectPath, displayName) {}

            columns: [
                AudioListView.ColumnItem {
                    id: modifiedColumn

                    width: function (parentWidth) {
                        let parentWidthExclusingSpacing = parentWidth - listView.columns.length * listView.view.columnSpacing;
                        return 0.15 * parentWidthExclusingSpacing
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

                AudioListView.ColumnItem {
                    id: previewColumn

                    width: function (parentWidth) {
                        let parentWidthExclusingSpacing = parentWidth - listView.columns.length * listView.view.columnSpacing;
                        return 0.7 * parentWidthExclusingSpacing
                    }

                    delegate: Image {
                        source: "qrc:/resources/Waveform.svg"
                        horizontalAlignment: Image.AlignLeft
                        verticalAlignment: Image.AlignVCenter   
                    }
                }
            ]
        }
    }

    Component {
        id: errorComp

        Item {
            anchors.fill: parent

            Message {
                anchors.top: parent.top
                anchors.topMargin: Math.max(parent.height / 3 - height / 2, 0)
                anchors.left: parent.left
                anchors.leftMargin: root.sideMargin
                anchors.right: parent.right
                anchors.rightMargin: root.sideMargin

                title: qsTrc("project", "Unable to load online files")
                body: qsTrc("global", "Please check your internet connection or try again later.")
            }
        }
    }

    Component {
        id: emptyComp

        Item {
            anchors.fill: parent

            Message {
                anchors.top: parent.top
                anchors.topMargin: Math.max(parent.height / 3 - height / 2, 0)
                anchors.left: parent.left
                anchors.leftMargin: root.sideMargin
                anchors.right: parent.right
                anchors.rightMargin: root.sideMargin

                title: qsTrc("project", "You don't have any online files yet")
                body: qsTrc("project", "Files will appear here when you save a file to the cloud, or publish a project")
            }
        }
    }

    Component {
        id: notSignedInComp

        Item {
            anchors.fill: parent

            Message {
                anchors.top: parent.top
                anchors.topMargin: Math.max(parent.height / 3 - height / 2, 0)
                anchors.left: parent.left
                anchors.leftMargin: root.sideMargin
                anchors.right: parent.right
                anchors.rightMargin: root.sideMargin

                title: qsTrc("project", "You are not signed in")
                body: qsTrc("project", "Please sign in to view your online files")
            }
        }
    }
}
