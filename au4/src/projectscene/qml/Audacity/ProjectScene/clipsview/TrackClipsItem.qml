import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Item {

    id: root

    property alias trackId: clipsModel.trackId
    property alias context: clipsModel.context

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
        clipsModel.load()
    }

    Item {
        anchors.fill: parent
        anchors.bottomMargin: sep.height

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
                    model.clipTitle = newTitle
                    clipsModel.resetSelectedClip()
                }

                onTitleEditCanceled: {
                    clipsModel.resetSelectedClip()
                }
            }
        }
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

    SeparatorLine { id: sep; anchors.bottom: parent.bottom }
}
