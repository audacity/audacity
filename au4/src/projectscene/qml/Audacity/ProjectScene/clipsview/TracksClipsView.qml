import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Rectangle {

    id: root

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

    MouseArea {
        anchors.fill: parent

        onWheel: function(wheel) {
            wheel.accepted = timeline.onWheel(wheel.angleDelta.y)
        }

        onClicked: function(mouse) {
            console.log("mouse.x: " + mouse.x)
            playCursor.seekToX(mouse.x)
        }
    }

    PlayCursor {
        id: playCursor
        anchors.top: view.top
        anchors.bottom: parent.bottom
        context: timeline.context
    }
}
