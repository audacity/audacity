import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Item {

    id: root

    Component.onCompleted: {
        tracksModel.load()
    }

    TracksListClipsModel {
        id: tracksModel
    }


    function timeToPx(msec) {
        return msec * 1;
    }

    Flickable {
        anchors.fill: parent

        contentHeight: parent.height
        contentWidth: root.timeToPx(2000)

        Item {
            id: topGap
            width: 1
            height: 76
        }

        StyledListView {
            id: view

            anchors.top: topGap.bottom
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.bottom: parent.bottom

            model: tracksModel

            delegate: TrackClipsItem {
                trackId: trackIdData
            }
        }
    }
}
