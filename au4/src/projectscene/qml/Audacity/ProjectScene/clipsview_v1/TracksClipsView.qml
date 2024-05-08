import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Qt.labs.qmlmodels

import Muse.UiComponents
import Muse.Ui

import Audacity.ProjectScene

TimelineContext
{
    id: view

    tracksOriginOffset: 0//280

    TracksListClipsModel {
        id: trackListClipsModel
    }

    Component.onCompleted: {
        trackListClipsModel.load()
    }


    Item {
        id: header

        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right

        height: 32
    }

    ListView {
        id: trackListView

        anchors.top: header.bottom
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom

        clip: true

        pixelAligned: true

        flickableDirection: Flickable.VerticalFlick

        model: trackListClipsModel
        delegate: WaveTrackView {

            anchors.left: parent.left
            anchors.right: parent.right
            height: 126
            context : view
            track : trackData
        }
    }
}
