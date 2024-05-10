import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Rectangle {

    id: root

    clip: true

    color: ui.theme.backgroundPrimaryColor

    TracksListClipsModel {
        id: tracksModel
    }

    Component.onCompleted: {
        tracksModel.load()
    }

    Timeline {
        id: timeline

        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right
    }

    MouseArea {
        anchors.fill: parent
        onWheel: function(wheel) {
            timeline.onWheel(wheel.angleDelta.y)
        }
    }

    Column {
        anchors.top: timeline.bottom
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom

        Rectangle {
            height: 44
            width: parent.width

            StyledTextLabel {
                anchors.fill: parent
                anchors.leftMargin: 16
                font: ui.theme.bodyBoldFont
                text: "zoom: " + timeline.context.zoom + ", offset: " + timeline.context.offset
            }
        }

        Repeater {
            model: tracksModel
            delegate: TrackClipsItem {
                anchors.left: parent.left
                anchors.right: parent.right
                height: 144
                context: timeline.context
                trackId: trackIdData
            }
        }
    }
}
