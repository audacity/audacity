import QtQuick
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene
import Audacity.Project

Rectangle {

    id: root

    property bool clipHovered: false
    property bool clipHeaderHovered: false
    property var hoveredClipKey: null
    property bool tracksHovered: false
    property bool underSelection: false

    color: ui.theme.backgroundPrimaryColor

    clip:true

    TracksListClipsModel {
        id: tracksModel

        onTotalTracksHeightChanged: {
            timeline.context.onResizeFrameContentHeight(tracksModel.totalTracksHeight)
        }
    }

    ProjectPropertiesModel {
        id: project

        onCaptureThumbnail: function captureThumbnail(thumbnailUrl) {
            // hide playCursor for the time grabbing image
            playCursor.visible = false
            content.grabToImage(function(result) {
                playCursor.visible = true
                var success = result.saveToFile(thumbnailUrl)
                project.onThumbnailCreated(success)
            })
        }
    }

    SelectionContextMenuModel {
        id: selectionContextMenuModel
    }

    ContextMenuLoader {
        id: selectionContextMenuLoader

        onHandleMenuItem: function(itemId) {
            selectionContextMenuModel.handleMenuItem(itemId)
        }
    }

    CanvasContextMenuModel {
        id: canvasContextMenuModel
    }

    ContextMenuLoader {
        id: canvasContextMenuLoader

        onHandleMenuItem: function(itemId) {
            canvasContextMenuModel.handleMenuItem(itemId)
        }
    }

    //! NOTE Sync with TracksPanel
    TracksViewStateModel {
        id: tracksViewState
        onTracksVericalYChanged: {
            tracksClipsView.contentY = tracksViewState.tracksVericalY
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

        onSelectionStarted: {
            underSelection = true
        }

        onSelectionEnded: {
            underSelection = false
        }
    }

    Component.onCompleted: {
        //! NOTE Models depend on geometry, so let's create a page first and then initialize the models
        Qt.callLater(root.init)

        selectionContextMenuModel.load()
        canvasContextMenuModel.load()
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

        clip: true

        height: 40

        function updateCursorPosition(x, y) {
            lineCursor.x = x
            timeline.context.updateMousePositionTime(x)
            tracksViewState.setMouseY(Math.max(0, Math.min(y, mainMouseArea.height)));
        }

        MouseArea {
            id: timelineMouseArea
            anchors.fill: parent
            hoverEnabled: true

            onPositionChanged: function(e) {
                timeline.updateCursorPosition(e.x, e.y)
            }

            onClicked: function (e) {
                if (!timeline.isMajorSection(e.y)) {
                    playCursorController.seekToX(e.x, true /* triggerPlay */)
                }
            }
        }

        Rectangle {
            id: lineCursor

            y: parent.top
            height: timeline.height
            width: 1

            color: ui.theme.fontPrimaryColor
        }

        Rectangle {
            id: timelineSelRect

            x: timeline.context.singleClipSelected ? timeline.context.selectedClipStartPosition : timeline.context.selectionStartPosition
            width: timeline.context.singleClipSelected ? timeline.context.selectedClipEndPosition - x : timeline.context.selectionEndPosition - x

            anchors.top: parent.top
            anchors.bottom: parent.bottom

            color: "#ABE7FF"
            opacity: 0.3
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

        GridLines {
            timelineRuler: timeline.ruler
            anchors.fill: parent
        }

        MouseArea {
            id: mainMouseArea
            anchors.fill: parent

            preventStealing: true
            acceptedButtons: Qt.LeftButton | Qt.RightButton

            hoverEnabled: true

            onWheel: function(wheelEvent) {
                timeline.onWheel(wheelEvent.x, wheelEvent.pixelDelta, wheelEvent.angleDelta)
            }

            onPressed: function(e) {
                if (e.button === Qt.LeftButton) {
                    if (root.clipHeaderHovered) {
                        tracksClipsView.clipStartEditRequested(hoveredClipKey)
                    } else {
                        if (!(e.modifiers & (Qt.ControlModifier | Qt.ShiftModifier))) {
                            playCursorController.seekToX(e.x)
                        }
                        selectionController.onPressed(e.x, e.y)
                        selectionController.resetSelectedClip()
                        clipsSelection.visible = true
                    }
                } else if (e.button === Qt.RightButton) {
                    if (tracksHovered) {
                        //! TODO AU4: handle context menu over empty track area
                    } else {
                        canvasContextMenuLoader.show(Qt.point(e.x + timelineIndent.width, e.y + timeline.height), canvasContextMenuModel.items)
                    }
                }
            }
            onPositionChanged: function(e) {
                timeline.updateCursorPosition(e.x, e.y)

                if (root.clipHeaderHovered && pressed) {
                    tracksClipsView.clipMoveRequested(hoveredClipKey, false)
                    tracksClipsView.startAutoScroll()
                } else {
                    selectionController.onPositionChanged(e.x, e.y)

                    if (root.clipHovered && !tracksClipsView.moveActive) {
                        root.clipHovered = false
                    }
                }
            }
            onReleased: e => {
                if (e.button !== Qt.LeftButton) {
                    return
                }

                if (root.clipHeaderHovered && tracksClipsView.moveActive) {
                    tracksClipsView.clipMoveRequested(hoveredClipKey, true)
                    tracksClipsView.stopAutoScroll()
                } else {
                    if (selectionController.isLeftSelection(e.x)) {
                        playCursorController.seekToX(e.x)
                    }
                    selectionController.onReleased(e.x, e.y)
                    if (e.modifiers & (Qt.ControlModifier | Qt.ShiftModifier)) {
                        playCursorController.seekToX(timeline.context.selectionStartPosition)
                    }

                    playCursorController.setPlaybackRegion(timeline.context.selectionStartPosition, timeline.context.selectionEndPosition)

                    clipsSelection.visible = false
                }
            }

            onClicked: e => {
                if (e.button !== Qt.LeftButton) {
                    return
                }

                if (!root.clipHovered) {
                    selectionController.resetSelectedClip()
                }
            }

            onDoubleClicked: e => {
                if (e.button !== Qt.LeftButton) {
                    return
                }
                if (root.clipHovered) {
                    selectionController.selectClipAudioData(root.hoveredClipKey)
                    playCursorController.setPlaybackRegion(timeline.context.selectedClipStartPosition, timeline.context.selectedClipEndPosition)
                } else {
                    selectionController.selectTrackAudioData(e.y)
                    playCursorController.setPlaybackRegion(timeline.context.selectedClipStartPosition, timeline.context.selectedClipEndPosition)
                }
                clipsSelection.visible = false
            }
        }

        StyledViewScrollAndZoomArea {
            id: tracksClipsViewArea

            anchors.fill: parent

            view: tracksClipsView

            horizontalScrollbarSize: timeline.context.horizontalScrollbarSize
            startHorizontalScrollPosition: timeline.context.startHorizontalScrollPosition

            verticalScrollbarSize: timeline.context.verticalScrollbarSize
            startVerticalScrollPosition: timeline.context.startVerticalScrollPosition

            StyledListView {
                id: tracksClipsView

                anchors.fill: parent
                clip: true

                property bool moveActive: false

                ScrollBar.horizontal: null
                ScrollBar.vertical: null

                property real lockedVerticalScrollPosition
                property bool verticalScrollLocked: tracksViewState.tracksVerticalScrollLocked

                signal clipMoveRequested(var clipKey, bool completed)
                signal clipStartEditRequested(var clipKey)
                signal startAutoScroll()
                signal stopAutoScroll()

                footer: Item {
                    height: tracksViewState.tracksVerticalScrollPadding
                }

                onVerticalScrollLockedChanged: {
                    lockedVerticalScrollPosition = contentY
                }

                onContentYChanged: {
                    if (verticalScrollLocked) {
                        contentY = lockedVerticalScrollPosition
                    }
                    else {
                        tracksViewState.changeTracksVericalY(tracksClipsView.contentY)
                        timeline.context.startVerticalScrollPosition = tracksClipsView.contentY
                    }
                }

                onHeightChanged: {
                    timeline.context.onResizeFrameHeight(tracksClipsView.height)
                }

                Connections {
                    target: timeline.context

                    function onViewContentYChangeRequested(delta) {
                        let totalContentHeight = tracksModel.totalTracksHeight + tracksViewState.tracksVerticalScrollPadding
                        let canMove = totalContentHeight > tracksClipsView.height
                        if (!canMove) {
                            return
                        }

                        let contentYOffset = tracksClipsView.contentY + delta

                        let maxContentY = totalContentHeight - tracksClipsView.height
                        maxContentY = Math.max(maxContentY, tracksClipsView.contentY)
                        contentYOffset = Math.max(Math.min(contentYOffset, maxContentY), 0)

                        tracksClipsView.contentY = contentYOffset
                    }
                }

                interactive: false

                model: tracksModel

                delegate: TrackClipsItem {
                    width: tracksClipsView.width
                    context: timeline.context
                    container: tracksClipsView
                    canvas: content
                    trackId: model.trackId
                    isDataSelected: model.isDataSelected
                    isTrackSelected: model.isTrackSelected
                    isMultiSelectionActive: model.isMultiSelectionActive
                    moveActive: tracksClipsView.moveActive
                    underSelection: root.underSelection

                    onTrackItemMousePositionChanged: function(xWithinTrack, yWithinTrack, clipKey) {
                        let xGlobalPosition = xWithinTrack
                        let yGlobalPosition = y + yWithinTrack - tracksClipsView.contentY
                        timeline.updateCursorPosition(xGlobalPosition, yGlobalPosition)

                        if (!root.clipHovered) {
                            root.clipHovered = true
                        }
                        root.hoveredClipKey = clipKey
                    }

                    onSetHoveredClipKey: function(clipKey) {
                        root.hoveredClipKey = clipKey
                    }

                    onClipHeaderHoveredChanged: function(val) {
                        root.clipHeaderHovered = val
                    }

                    onClipSelectedRequested: {
                        selectionController.resetDataSelection()
                        clipsSelection.visible = false
                    }

                    onSelectionResetRequested: {
                        selectionController.resetDataSelection()
                    }

                    onUpdateMoveActive: function(completed) {
                        if (tracksClipsView.moveActive !== completed) {
                            return;
                        }
                        tracksClipsView.moveActive = !completed;
                    }

                    onRequestSelectionContextMenu: function(x, y) {
                        selectionContextMenuLoader.show(Qt.point(x + canvasIndent.width, y + timeline.height), selectionContextMenuModel.items)
                    }

                    onSelectionDraged: function(x1, x2, completed) {
                        selectionController.onSelectionDraged(x1, x2, completed)
                    }

                    onSeekToX: function(x) {
                        playCursorController.seekToX(x)
                    }

                    onInteractionStarted: {
                        tracksViewState.requestVerticalScrollLock()
                    }

                    onInteractionEnded: {
                        tracksViewState.requestVerticalScrollUnlock()
                    }
                }

                HoverHandler {
                    property bool isNeedSelectionCursor: !selectionController.selectionActive
                    cursorShape: isNeedSelectionCursor ? Qt.IBeamCursor : Qt.ArrowCursor

                    onHoveredChanged: {
                        root.tracksHovered = hovered
                    }
                }
            }

            onPinchToZoom: function(scale, pos) {
                timeline.context.pinchToZoom(scale, pos)
            }

            onScrollHorizontal: function(newPos) {
                timeline.context.scrollHorizontal(newPos)
            }

            onScrollVertical: function(newPos) {
                timeline.context.scrollVertical(newPos)
            }
        }

        Rectangle {
            id: clipsSelection

            anchors.top: parent.top
            anchors.bottom: parent.bottom
            color: "#ABE7FF"
            opacity: 0.05
            visible: false

            x: Math.max(timeline.context.selectionStartPosition, 0.0)
            width: timeline.context.selectionEndPosition - x
        }

        PlayCursor {
            id: playCursor
            anchors.top: tracksClipsViewArea.top
            anchors.bottom: parent.bottom
            x: playCursorController.positionX
            z: 2
            timelinePressed: timelineMouseArea.pressed

            onSetPlaybackPosition: function(ix) {
                playCursorController.seekToX(ix)
            }

            onPlayCursorMousePositionChanged: function(ix) {
                timeline.updateCursorPosition(ix, -1)
            }
        }

        VerticalRulersPanel {
            id: verticalRulers

            height: parent.height - timeline.height
            anchors.right: tracksClipsViewArea.right
            anchors.bottom: tracksClipsViewArea.bottom

            visible: tracksModel.isVerticalRulersVisible
        }
    }
}
