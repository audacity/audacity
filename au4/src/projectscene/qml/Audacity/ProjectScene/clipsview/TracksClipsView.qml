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

    //! NOTE Sync with TracksPanel
    TracksViewStateModel {
        id: tracksViewState
        onTracksVericalYChanged: {
            if (!view.moving) {
                view.contentY = tracksViewState.tracksVericalY
            }
        }
    }

    Component.onCompleted: {
        tracksViewState.init()
        tracksModel.load()
    }

    Timeline {
        id: timeline

        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right

        height: 76
        z: 1
    }

    MouseArea {
        anchors.fill: parent
        onWheel: function(wheel) {
            wheel.accepted = timeline.onWheel(wheel.angleDelta.y)
        }
    }

    StyledListView {
        id: view

        anchors.top: timeline.bottom
        anchors.bottom: parent.bottom
        anchors.left: parent.left
        anchors.right: parent.right

        onContentYChanged: {
            tracksViewState.changeTracksVericalY(view.contentY)
        }

        interactive: true

        model: tracksModel

        delegate: TrackClipsItem {
            anchors.left: parent.left
            anchors.right: parent.right
            context: timeline.context
            trackId: trackIdData

            onInteractionStarted: {
                view.interactive = false
            }

            onInteractionEnded: {
                view.interactive = true
            }
        }
    }
}
