import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Rectangle {

    id: root

    clip: true

    //color: ui.theme.backgroundPrimaryColor
    color: "#ffffff"

    TracksListClipsModel {
        id: tracksModel
    }

    Component.onCompleted: {
        tracksModel.load()
    }

    Rectangle {
        anchors.fill: parent
        anchors.leftMargin: 150
        anchors.rightMargin: 150
        color: ui.theme.backgroundPrimaryColor

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
}
