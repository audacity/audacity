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
        cloudAudioFilesModel.reload()
    }

    CloudAudioFilesModel {
        id: cloudAudioFilesModel
    }

    Component.onCompleted: {
        cloudAudioFilesModel.load()
    }

    QtObject {
        id: prv
        property string gridPlaceholderFile: "qrc:/resources/AudioFilePlaceholder.svg"
    }

    sourceComponent: root.viewType === ProjectsPageModel.List ? listComp : gridComp

    Component {
        id: gridComp

        ProjectsGridView {
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
            id: list

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
                        let parentWidthExclusingSpacing = parentWidth - list.columns.length * list.view.columnSpacing;
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
                        let parentWidthExclusingSpacing = parentWidth - list.columns.length * list.view.columnSpacing;
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
}