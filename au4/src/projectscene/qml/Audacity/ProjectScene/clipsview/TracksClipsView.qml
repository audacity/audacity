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

    SelectionViewController {
        id: selectionController
        context: timeline.context

        onSelectionStarted: clipsSelection.onSelectionStarted()
        onSelectionChanged: function(p1, p2) { clipsSelection.onSelectionChanged(p1, p2) }
        onSelectionEnded: function(p1, p2) { clipsSelection.onSelectionEnded(p1, p2) }
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
            property bool isNeedSelectionCursor: !selectionController.selectionActive && mainMouseArea.mouseOnTracks
            hoverEnabled: true
            onContainsMouseChanged: {
                if (!containsMouse) {
                    mouseOnTracks = false
                }
            }
            onIsNeedSelectionCursorChanged: {
                if (isNeedSelectionCursor) {
                    tracksViewState.setOverrideCursor(Qt.IBeamCursor)
                } else {
                    tracksViewState.resetOverrideCursor()
                }
            }

            onWheel: function(wheelEvent) {
                timeline.onWheel(wheelEvent.pixelDelta, wheelEvent.angleDelta)
            }

            onPressed: e => selectionController.onPressed(e.x, e.y)
            onPositionChanged: function(e) {
                mouseOnTracks = e.y < view.visibleContentHeight
                selectionController.onPositionChanged(e.x, e.y)
            }
            onReleased: e => selectionController.onReleased(e.x, e.y)
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

            onHeightChanged: {
                timeline.context.onResizeFrameHeight(view.height)
            }

            Connections {
                target: timeline.context

                function onShiftViewByY(shift) {
                    if (shift > 0) {
                        view.flick(0, view.maximumFlickVelocity)
                    } else if (shift < 0) {
                        view.flick(0, -view.maximumFlickVelocity)
                    }
                }
            }

            interactive: false

            model: tracksModel

            delegate: TrackClipsItem {
                anchors.left: parent.left
                anchors.right: parent.right
                context: timeline.context
                trackId: model.trackId
                isDataSelected: model.isDataSelected
            }
        }

        ClipsSelection {
            id: clipsSelection
            anchors.fill: parent
            onSelectionDraged: function(x1, x2, completed) { selectionController.onSelectionDraged(x1, x2, completed) }
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
