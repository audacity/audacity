/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Item {
    id: root

    property NavigationSection navigationSection: null
    property NavigationPanel navigationPanel: view.count > 0 ? view.itemAtIndex(0).navigationPanel : null // first panel
    property alias tracksModel: tracksModel
    // property alias contextMenuModel: contextMenuModel

    TracksListModel {
        id: tracksModel
    }

    //! NOTE Sync with TracksClipsView
    TracksViewStateModel {
        id: tracksViewState
        onTracksVericalYChanged: {
            view.contentY = tracksViewState.tracksVericalY
        }
    }

    Component.onCompleted: {
        tracksViewState.init()
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

        StyledListView {
            id: view
            Layout.topMargin: 1
            Layout.fillWidth: true
            Layout.fillHeight: true

            spacing: 0
            cacheBuffer: 3000

            ScrollBar.vertical: null
            onContentYChanged: {
                tracksViewState.changeTracksVericalY(view.contentY)
            }

            interactive: true

            model: tracksModel

            navigation.section: root.navigationSection
            navigation.order: 1

            delegate: TrackItem {
                item: itemData

                isSelected: Boolean(item) ? item.isSelected : false

                navigation.name: Boolean(item) ? item.title + item.index : ""
                navigation.panel: view.navigation
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

                onInteractionStarted: {
                    view.interactive = false
                }

                onInteractionEnded: {
                    view.interactive = true
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
