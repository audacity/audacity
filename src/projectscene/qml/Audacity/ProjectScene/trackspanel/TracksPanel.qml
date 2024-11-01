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
    signal openEffectsRequested()

    // property alias contextMenuModel: contextMenuModel
    property int effectsSectionWidth: 240 // TODO: can this be set as a constant that can be imported?
    property alias showEffectsSection: trackEffectsSection.visible
    property alias selectedTrackIndex: trackEffectsSection.selectedTrackIndex

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

    RowLayout {

        anchors.fill: parent
        spacing: 0

        TrackEffectsSection {
            id: trackEffectsSection
            Layout.preferredWidth: root.effectsSectionWidth
            Layout.maximumWidth: root.effectsSectionWidth
            Layout.minimumWidth: root.effectsSectionWidth
            Layout.fillHeight: true
            visible: false
        }

        SeparatorLine { }

        ColumnLayout {
            id: contentColumn

            Layout.fillWidth: true
            Layout.fillHeight: true

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
                    onIsSelectedChanged: {
                        if (isSelected)
                            root.selectedTrackIndex = index
                    }

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

                    onSelectionRequested: {
                        tracksModel.selectRow(model.index, true)
                    }

                    onOpenEffectsRequested: {
                        root.selectedTrackIndex = index
                        root.openEffectsRequested()
                    }
                }
            }
        }
    }
}
