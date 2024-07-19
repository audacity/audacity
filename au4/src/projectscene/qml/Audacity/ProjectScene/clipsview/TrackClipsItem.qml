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

        Repeater {
            id: repeator

            model: clipsModel

            delegate: ClipItem {

                property QtObject item: model.item

                height: parent.height
                width: item.width
                x: item.x

                context: root.context
                title: item.title
                clipColor: item.color
                clipKey: item.key
                clipSelected: clipsModel.selectedClipIdx === model.index
                collapsed: trackViewState.isTrackCollapsed

                dragMaximumX: item.moveMaximumX + borderWidth
                dragMinimumX: item.moveMinimumX - borderWidth

                onPositionChanged: function(x) {
                    clipsModel.modeClip(model.index, x)
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
