import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

TimelineContext
{
    id: root

    TracksListClipsModel {
        id: tracksModel
    }

    Component.onCompleted: {
        tracksModel.load()
    }


    Item {
        id: header

        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right

        height: 76

        SeparatorLine { anchors.bottom: parent.bottom }
    }

    ListView {
        id: tracksView

        anchors.top: header.bottom
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
            context: root
            trackId: trackIdData
        }
    }
}
