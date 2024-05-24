import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Item {

    id: root

    property alias trackId: clipsModel.trackId
    property alias context: clipsModel.context

    height: trackViewState.trackHeight

    ClipsListModel {
        id: clipsModel
    }

    TrackViewStateModel {
        id: trackViewState
        trackId: root.trackId
    }

    Component.onCompleted: {
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

        onPositionChanged: function(mouse) {
            trackViewState.changeTrackHeight(mouse.y)
        }
    }

    SeparatorLine { anchors.bottom: parent.bottom }
}
