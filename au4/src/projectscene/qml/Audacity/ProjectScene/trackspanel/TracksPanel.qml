/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Item {
    id: root

    property NavigationSection navigationSection: null
    property NavigationPanel navigationPanel: view.count > 0 ? view.itemAtIndex(0).navigationPanel : null // first panel
    // property alias contextMenuModel: contextMenuModel

    TracksListModel {
        id: tracksModel
    }

    Component.onCompleted: {
        tracksModel.load()
    }

    QtObject {
        id: prv

        property string currentItemNavigationName: ""
    }

    ColumnLayout {
        id: contentColumn

        anchors.fill: parent

        readonly property int sideMargin: 12
        spacing: sideMargin

        TracksControlPanel {
            id: controlPanel
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignTop

            Layout.leftMargin: contentColumn.sideMargin
            Layout.rightMargin: contentColumn.sideMargin

            navigation.section: root.navigationSection
            navigation.order: 2

            isMovingUpAvailable: tracksModel.isMovingUpAvailable
            isMovingDownAvailable: tracksModel.isMovingDownAvailable
            isAddingAvailable: tracksModel.isAddingAvailable
            isRemovingAvailable: tracksModel.isRemovingAvailable

            onAddRequested: function(type, quantity) {
                tracksModel.addTracks(type, quantity)
            }

            onMoveUpRequested: {
                tracksModel.moveSelectedRowsUp()
            }

            onMoveDownRequested: {
                tracksModel.moveSelectedRowsDown()
            }

            onRemovingRequested: {
                tracksModel.removeSelectedRows()
            }
        }

        StyledListView {
            id: view

            Layout.fillWidth: true
            Layout.fillHeight: true

            spacing: 0

            model: tracksModel

            interactive: height < contentHeight

            SeparatorLine {
                anchors.top: parent.top
            }

            delegate: TrackItem {
                item: itemData

                isSelected: Boolean(item) ? item.isSelected : false

                navigation.name: Boolean(item) ? item.title + item.index : ""
                navigationPanel.section: root.navigationSection
                navigation.row: model.index
                navigation.accessible.name: Boolean(item) ? item.title : ""
                navigation.onActiveChanged: {
                    if (navigation.active) {
                        prv.currentItemNavigationName = navigation.name
                        view.positionViewAtIndex(index, ListView.Contain)
                    }
                }

                onClicked: {
                    tracksModel.selectRow(model.index)
                }

                onDuplicateRequested: {
                    tracksModel.duplicateTrack(model.index)
                }

                onDeleteRequested: {
                    tracksModel.deleteTrack(model.index)
                }

                onOpenEffectsRequested: {
                        tracksModel.openEffectsForTrack(model.index)
                }
            }
        }
    }
}
