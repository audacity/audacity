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
    property alias showEffectsSection: effectSectionModel.showEffectsSection
    property int selectedTrackIndex: -1

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
        effectSectionModel.load()
    }

    QtObject {
        id: prv

        property string currentItemNavigationName: ""
    }

    RowLayout {

        anchors.fill: parent
        spacing: 0

        ColumnLayout {

            RealtimeEffectSectionModel {
                id: effectSectionModel
            }

            id: effectColumn
            Layout.preferredWidth: root.effectsSectionWidth
            Layout.maximumWidth: root.effectsSectionWidth
            Layout.minimumWidth: root.effectsSectionWidth
            Layout.fillHeight: true
            visible: effectSectionModel.showEffectsSection
            spacing: 0

            SeparatorLine { }

            TrackEffectsSection {
                id: trackEffectsSection
                isMasterTrack: false
                Layout.fillWidth: true
                Layout.fillHeight: true
                Layout.minimumHeight: trackEffectsSection.minimumHeight
            }

            SeparatorLine {
                id: separator
                MouseArea {
                    id: mouseArea
                    anchors.left: parent.left
                    anchors.right: parent.right
                    anchors.verticalCenter: parent.verticalCenter
                    height: 10
                    cursorShape: Qt.SizeVerCursor

                    property real startY: 0
                    property real startHeight: 0

                    onPressed: (mouse) => {
                        startY = mouse.y
                        startHeight = trackEffectsSection.height
                    }

                    onPositionChanged: (mouse) => {
                        const deltaY = mouse.y - startY
                        const newMasterHeight = masterEffectsSection.height - deltaY
                        masterEffectsSection.Layout.preferredHeight = Math.min(newMasterHeight, effectColumn.height - trackEffectsSection.minimumHeight)
                    }
                }
            }

            TrackEffectsSection {
                id: masterEffectsSection
                isMasterTrack: true
                Layout.fillWidth: true
                Layout.preferredHeight: 300
                Layout.minimumHeight: masterEffectsSection.minimumHeight
            }
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

                property real lockedVerticalScrollPosition
                property bool verticalScrollLocked: tracksViewState.tracksVerticalScrollLocked

                onVerticalScrollLockedChanged: {
                    lockedVerticalScrollPosition = contentY
                }

                onContentYChanged: {
                    if (verticalScrollLocked) {
                        view.contentY = lockedVerticalScrollPosition
                    }
                    else {
                        tracksViewState.changeTracksVericalY(view.contentY)
                    }
                }

                interactive: false

                model: tracksModel

                footer: Item {
                    height: tracksViewState.tracksVerticalScrollPadding
                }

                navigation.section: root.navigationSection
                navigation.order: 1
                delegate: TrackItem {
                    item: itemData
                    isSelected: Boolean(item) ? item.isSelected : false
                    container: view

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

                    onInteractionStarted: {
                        tracksViewState.requestVerticalScrollLock()
                    }

                    onInteractionEnded: {
                        tracksViewState.requestVerticalScrollUnlock()
                    }

                    onSelectionRequested: function (exclusive) {
                        tracksModel.selectRow(model.index, exclusive)
                    }

                    onOpenEffectsRequested: {
                        effectSectionModel.showEffectsSection = true
                        root.openEffectsRequested()
                    }

                    Component.onCompleted: {
                        mousePressed.connect(dragHandler.startDrag)
                        mouseReleased.connect(dragHandler.endDrag)
                        mouseMoved.connect(dragHandler.onMouseMove)
                    }
                }

                Item {
                    id: wheelHandler

                    anchors.fill: parent

                    WheelHandler {
                        acceptedDevices: PointerDevice.Mouse | PointerDevice.TouchPad

                        onWheel: function(wheelEvent) {
                            let delta = wheelEvent.pixelDelta.y !== 0 ? wheelEvent.pixelDelta.y : wheelEvent.angleDelta.y
                            let offset  = view.contentY - delta

                            let maxContentY = view.contentHeight - view.height
                            maxContentY = Math.max(maxContentY, view.contentY)
                            offset = Math.max(Math.min(offset, maxContentY), 0)

                            view.contentY = offset
                        }
                    }
                }

                Item {
                    id: dragHandler

                    anchors.fill: parent

                    property bool dragging: false
                    property int dragFirstIndex: -1
                    property int dragLastIndex: -1
                    property int dropIndex: -1

                    Rectangle {
                        id: dropCursor
                        width: parent.width
                        height: 2
                        color: ui.theme.accentColor
                        visible: dragHandler.dropIndex >= 0
                        y: dragHandler.dropIndex >= 0 && dragHandler.dropIndex < view.count
                        ? view.itemAtIndex(dragHandler.dropIndex).y - view.contentY
                        : view.contentHeight - view.contentY - tracksViewState.tracksVerticalScrollPadding
                    }

                    function startDrag(item, mouseX, mouseY) {
                        let selection = tracksModel.selectionModel().selectedIndexes
                        if (selection.length === 0) {
                            return
                        }

                        mouseY = view.mapFromItem(item, mouseX, mouseY).y + view.contentY

                        dragFirstIndex = selection[0].row
                        dragLastIndex = selection[selection.length - 1].row

                        dragging = true
                    }

                    function endDrag() {
                        dragging = false
                        setDraggedStateForTracks(false)

                        if (dragFirstIndex < dropIndex) {
                            dropIndex--
                        }

                        if (dragFirstIndex == dropIndex || dropIndex < 0) {
                            return
                        }

                        let selectedRows = tracksModel.selectionModel().selectedIndexes.map(index => index.row)

                        tracksModel.requestTracksMove(selectedRows, dropIndex)
                        dropIndex = -1
                    }

                    function onMouseMove(item, mouseX, mouseY) {
                        if (!dragging) {
                            return
                        }

                        mouseY = view.mapFromItem(item, mouseX, mouseY).y + view.contentY

                        let itemAtCursor = view.itemAt(0, mouseY)
                        let indexAtCursor = view.indexAt(0, mouseY)

                        if (itemAtCursor) {
                            if (itemAtCursor.height / 2 < view.mapToItem(itemAtCursor, 0, mouseY - view.contentY).y) {
                                indexAtCursor++
                            }
                        }
                        else {
                            indexAtCursor = mouseY < 0 ? 0 : view.count
                        }

                        if (dragFirstIndex > indexAtCursor || (dragLastIndex + 1) < indexAtCursor ) {
                            dropIndex = indexAtCursor
                            setDraggedStateForTracks(true)
                        }
                        else {
                            dropIndex = -1
                            setDraggedStateForTracks(false)
                        }
                    }

                    function setDraggedStateForTracks(state) {
                        tracksModel.selectionModel().selectedIndexes.forEach(selectedIndex => {
                            view.itemAtIndex(selectedIndex.row).dragged = state
                        })
                    }
                }
            }
        }
    }
}
