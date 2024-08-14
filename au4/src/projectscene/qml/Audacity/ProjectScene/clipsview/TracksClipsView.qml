import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene
import Audacity.Project

Rectangle {

    id: root

    property bool clipHovered: false
    color: ui.theme.backgroundPrimaryColor

    TracksListClipsModel {
        id: tracksModel
    }

    ProjectPropertiesModel {
        id: project

        onCaptureThumbnail: function captureThumbnail() {
            // hide playCursor for the time grabbing image
            playCursor.visible = false
            content.grabToImage(function(result) {
                playCursor.visible = true
                result.saveToFile(project.thumbnailUrl)
            })
        }
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

    PlayPositionActionController {
        id: playPositionActionController
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
        //! NOTE Models depend on geometry, so let's create a page first and then initialize the models
        Qt.callLater(root.init)
    }

    function init() {
        timeline.init()
        playCursorController.init()
        playPositionActionController.init()
        tracksViewState.init()
        project.init()
        //! NOTE Loading tracks, or rather clips, is the most havy operation.
        // Let's make sure that everything is loaded and initialized before this,
        // to avoid double loading at the beginning, when some parameters are initialized.
        Qt.callLater(tracksModel.load)
    }

    Rectangle {
        id: timelineIndent
        anchors.top: parent.top
        anchors.left: parent.left
        height: timeline.height
        width: content.anchors.leftMargin
        color: timeline.color

        SeparatorLine {
            id: topBorder
            width: parent.width
            anchors.bottom: parent.bottom
            color: ui.theme.strokeColor
        }
    }

    Rectangle {
        id: canvasIndent
        anchors.top: timelineIndent.bottom
        anchors.bottom: parent.bottom
        height: timeline.height
        width: content.anchors.leftMargin
        color: ui.theme.backgroundTertiaryColor
    }

    Timeline {
        id: timeline

        anchors.top: parent.top
        anchors.left: timelineIndent.right
        anchors.right: parent.right

        height: 77

        MouseArea {
            id: timelineMouseArea
            anchors.fill: parent
            hoverEnabled: true

            onPositionChanged: function(e) {
                lineCursor.x = e.x
                if (pressed) {
                    playCursorController.seekToX(e.x)
                }
            }

            onClicked: function (e) {
                playCursorController.seekToX(e.x)
            }
        }

        Rectangle {
            id: lineCursor

            y: parent.top
            height: timeline.height
            width: 1

            color: ui.theme.fontPrimaryColor
        }
    }

    Rectangle {
        id: content
        objectName: "clipsView"
        anchors.leftMargin: 12
        anchors.top: timeline.bottom
        anchors.bottom: parent.bottom
        anchors.left: parent.left
        anchors.right: parent.right

        // anchors.leftMargin: 130
        // anchors.rightMargin: 130

        GridLines {
            timelineRuler: timeline.ruler
            anchors.fill: parent
        }

        MouseArea {
            id: mainMouseArea
            anchors.fill: parent

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

            onPressed: function(e) {
                playCursorController.seekToX(e.x)
                selectionController.onPressed(e.x, e.y)
                selectionController.resetSelectedClip()
            }
            onPositionChanged: function(e) {
                mouseOnTracks = e.y < view.visibleContentHeight
                selectionController.onPositionChanged(e.x, e.y)
                lineCursor.x = e.x
                if (root.clipHovered) {
                    root.clipHovered = false
                }
            }
            onReleased: e => selectionController.onReleased(e.x, e.y)

            onClicked: {
                if (!root.clipHovered) {
                    selectionController.resetSelectedClip()
                }
            }
        }

        StyledListView {
            id: view

            anchors.fill: parent
            clip: false

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
                width: view.width
                context: timeline.context
                trackId: model.trackId
                isDataSelected: model.isDataSelected

                onTrackItemMousePositionChanged: function(xWithinTrack, yWithinTrack) {
                    lineCursor.x = xWithinTrack
                    if (!root.clipHovered) {
                        root.clipHovered = true
                    }
                }

                onClipSelectedRequested: {
                    selectionController.resetDataSelection()
                    clipsSelection.active = false
                }
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
            z: 2
            timelinePressed: timelineMouseArea.pressed

            onSetPlaybackPosition: function(ix) {
                playCursorController.seekToX(ix)
            }

            onPlayCursorMousePositionChanged: function(ix) {
                lineCursor.x = ix
            }
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
