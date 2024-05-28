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
    clip: true

    ClipsListModel {
        id: clipsModel
    }

    TracksViewStateModel {
        id: trackViewState
        trackId: root.trackId
    }

    Component.onCompleted: {
        trackViewState.init()
        clipsModel.load()
    }

    Repeater {
        model: clipsModel

        delegate: ClipItem {

            height: parent.height
            width: model.clipWidthData
            x: model.clipLeftData

            title: model.clipTitleData

            context: root.context
            clipKey: model.clipKeyData

            onPositionChanged: function(x) {
                model.clipLeftData = x
            }
        }
    }

    ClipsSelection {
        id: clipsSelection

        anchors.fill: parent
        anchors.topMargin: 20 // clip header height

        onSelected: function(x1, x2) {
            clipsModel.onSelected(x1, x2)
        }

        onReset: clipsModel.resetSelection()
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

    SeparatorLine { anchors.bottom: parent.bottom }
}
