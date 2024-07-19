import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Item {

    id: root

    property alias trackId: clipsModel.trackId
    property alias context: clipsModel.context

    property bool isDataSelected: false

    signal interactionStarted()
    signal interactionEnded()

    height: trackViewState.trackHeight

    ClipsListModel {
        id: clipsModel

        onRequestClipTitleEdit: function(index){
            repeator.itemAt(index).editTitle()
        }
    }

    TracksViewStateModel {
        id: trackViewState
        trackId: root.trackId
    }

    Component.onCompleted: {
        trackViewState.init()
        clipsModel.init()
    }

    function changeClipTitle(index, newTitle) {
        clipsModel.changeClipTitle(index, newTitle)
        clipsModel.resetSelectedClip()
    }

    Item {
        anchors.fill: parent
        anchors.bottomMargin: sep.height
        z: 1

        Repeater {
            id: repeator

            model: clipsModel

            delegate: ClipItem {

                height: parent.height
                width: model.clipWidth
                x: model.clipLeft

                context: root.context
                title: model.clipTitle
                clipColor: model.clipColor
                clipKey: model.clipKey
                clipSelected: clipsModel.selectedClipIdx === model.index
                collapsed: trackViewState.isTrackCollapsed

                dragMaximumX: model.clipMoveMaximumX + borderWidth
                dragMinimumX: model.clipMoveMinimumX - borderWidth

                onPositionChanged: function(x) {
                    model.clipLeft = x
                }

                onRequestSelected: {
                    clipsModel.selectClip(model.index)
                }

                onTitleEditStarted: {
                    clipsModel.selectClip(model.index)
                }

                onTitleEditAccepted: function(newTitle) {
                    root.changeClipTitle(model.index, newTitle)
                }

                onTitleEditCanceled: {
                    clipsModel.resetSelectedClip()
                }
            }
        }
    }

    Rectangle {
        id: selRect
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        color: "#8EC9FF"
        opacity: 0.4
        visible: root.isDataSelected

        x: root.context.timeToPosition(root.context.selectionStartTime)
        width: root.context.timeToPosition(root.context.selectionEndTime) - x
        z: 1
    }

    Rectangle {
        id: selectedHighlight
        z: 0
        anchors.fill: parent
        color: ui.theme.white
        opacity: 0.05
        visible: root.isDataSelected
    }

    MouseArea {
        id: dragArea

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        height: 4

        cursorShape: Qt.SizeVerCursor

        onPressed: {
            root.interactionStarted()
        }

        onPositionChanged: function(mouse) {
            mouse.accepted = true
            trackViewState.changeTrackHeight(mouse.y)
        }

        onReleased: {
            root.interactionEnded()
        }
    }

    SeparatorLine {
        id: sep
        color: "#FFFFFF"
        opacity: 0.1
        anchors.bottom: parent.bottom }
}
