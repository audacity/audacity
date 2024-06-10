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

    PlayCursorController {
        id: playCursorController
        context: timeline.context
    }

    SelectionController {
        id: selectionController
        context: timeline.context

        onSelectedTracksChanged: {
            console.log("onSelectedTracksChanged: " + selectedTracks)
        }
    }

    Component.onCompleted: {
        playCursorController.init()
        tracksViewState.init()
        tracksModel.load()
    }

    Rectangle {
        id: timelineIndent
        anchors.top: parent.top
        anchors.left: parent.left
        height: timeline.height
        width: content.anchors.leftMargin
        color: timeline.color
    }

    Item {
        id: content
        anchors.fill: parent
        anchors.leftMargin: 8

        Timeline {
            id: timeline

            anchors.top: parent.top
            anchors.left: parent.left
            anchors.right: parent.right

            height: 76

            onClicked: function (e) {
                playCursorController.seekToX(e.x)
            }
        }

        MouseArea {
            id: mainMouseArea
            anchors.top: timeline.bottom
            anchors.bottom: parent.bottom
            anchors.left: parent.left
            anchors.right: parent.right

            property bool mouseOnTracks: false
            hoverEnabled: true
            onContainsMouseChanged: {
                if (!containsMouse) {
                    mouseOnTracks = false
                }
            }
            onMouseOnTracksChanged: {
                if (mouseOnTracks) {
                    tracksViewState.setOverrideCursor(Qt.IBeamCursor)
                } else {
                    tracksViewState.resetOverrideCursor()
                }
            }

            onWheel: function(wheel) {
                wheel.accepted = timeline.onWheel(wheel.angleDelta.y)
                if (!wheel.accepted) {
                    if (wheel.angleDelta.y > 0) {
                        view.flick(0, view.maximumFlickVelocity)
                    } else {
                        view.flick(0, -view.maximumFlickVelocity)
                    }
                }
            }

            onPressed: e => clipsSelection.onPressed(e)
            onPositionChanged: function(e) {
                mouseOnTracks = e.y < view.visibleContentHeight
                clipsSelection.onPositionChanged(e)
            }
            onReleased: e => clipsSelection.onReleased(e)
        }

        StyledListView {
            id: view

            anchors.top: timeline.bottom
            anchors.bottom: parent.bottom
            anchors.left: parent.left
            anchors.right: parent.right

            property real visibleContentHeight: view.contentHeight - view.contentY

            onContentYChanged: {
                tracksViewState.changeTracksVericalY(view.contentY)
            }

            interactive: false

            model: tracksModel

            delegate: TrackClipsItem {
                anchors.left: parent.left
                anchors.right: parent.right
                context: timeline.context
                trackId: trackIdData

                onInteractionStarted: {
                    // view.interactive = false
                }

                onInteractionEnded: {
                    // view.interactive = true
                }
            }
        }

        ClipsSelection {
            id: clipsSelection

            anchors.fill: parent

            onSelected: function(x1, y1, x2, y2) {
                // console.log("onSelected: x1: " + x1 + " y1: " + y1 + " x2: " + x2 + " y2: " + y2)
                //! NOTE The x coordinates must match the timeline.
                //! The y coordinates must match the track view
                //! If this is not the case, then appropriate adjustments must be made.
                selectionController.onSelectedCoords(x1, y1, x2, y2)
            }

            onReset: {
                selectionController.resetSelection()
            }
        }

        PlayCursor {
            id: playCursor
            anchors.top: view.top
            anchors.bottom: parent.bottom
            x: playCursorController.positionX
        }

        VerticalRulersPanel {
            id: verticalRulers

            height: parent.height - timeline.height
            anchors.right: view.right
            anchors.bottom: view.bottom

            visible: tracksModel.isVerticalRulersVisible
        }
    }
}
