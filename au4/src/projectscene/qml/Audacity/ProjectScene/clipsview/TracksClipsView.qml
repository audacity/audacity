import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Item {

    id: root

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

    ListView {
        id: tracksView

        anchors.top: timeline.bottom
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom

        clip: true

        pixelAligned: true

        flickableDirection: Flickable.VerticalFlick

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
