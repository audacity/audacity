import QtQuick
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene
import Audacity.Project
import Audacity.Playback

Rectangle {

    id: root

    property NavigationSection navigationSection: null

    property bool clipHovered: false
    property bool clipHeaderHovered: false
    property var hoveredClipKey: null
    property var hoveredTrackId: null
    property double hoveredTrackVerticalPosition
    property double hoveredTrackHeight
    property bool tracksHovered: false
    property bool guidelineActive: false
    property alias altPressed: tracksViewState.altPressed
    property alias ctrlPressed: tracksViewState.ctrlPressed
    property alias isSplitMode: tracksModel.isSplitMode
    property alias escPressed: tracksViewState.escPressed
    property double splitGuidelinePosition: -1

    readonly property string pencilShape: ":/images/customCursorShapes/Pencil.png"
    readonly property string smoothShape: ":/images/customCursorShapes/Smooth.png"
    readonly property string splitShape: ":/images/customCursorShapes/Split.png"
    readonly property string leftTrimShape: ":/images/customCursorShapes/ClipTrimLeft.png"
    readonly property string leftStretchShape: ":/images/customCursorShapes/ClipStretchLeft.png"
    readonly property string rightTrimShape: ":/images/customCursorShapes/ClipTrimRight.png"
    readonly property string rightStretchShape: ":/images/customCursorShapes/ClipStretchRight.png"

    color: ui.theme.backgroundPrimaryColor

    clip: true

    PlaybackStateModel {
        id: playbackState
    }

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
        onTracksVerticalOffsetChanged: {
            tracksClipsView.contentY = tracksViewState.tracksVerticalOffset
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

    PlayRegionController {
        id: playRegionController
        context: timeline.context
    }

    SelectionViewController {
        id: selectionController
        context: timeline.context
    }

    onEscPressedChanged: {
        if (escPressed) {
            tracksModel.isSplitMode = false
        }
    }

    Component.onCompleted: {
        //! NOTE Models depend on geometry, so let's create a page first and then initialize the models
        Qt.callLater(root.init)

        selectionController.load()
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

        //! NOTE setting verticalY has to be done after tracks are loaded,
        // otherwise project always starts at the very top
        Qt.callLater(() => tracksClipsView.contentY = tracksViewState.tracksVerticalOffset)
    }

    Rectangle {
        id: canvasIndent
        anchors.top: timelineHeader.bottom
        anchors.bottom: parent.bottom
        height: timelineHeader.height
        width: content.anchors.leftMargin
        color: ui.theme.backgroundQuarternaryColor
    }

    Item {
        id: timelineHeader

        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right
        height: timeline.height
        z: content.z + 1 // to prevent clips overlapping timeline ruler

        clip: true

        Rectangle {
            id: timelineIndent

            anchors.top: parent.top
            anchors.left: parent.left

            height: timeline.height
            width: content.anchors.leftMargin

            color: timeline.color

            SeparatorLine {
                id: topBorder

                anchors.bottom: parent.bottom

                width: parent.width

                color: ui.theme.strokeColor
            }
        }

        Timeline {
            id: timeline

            anchors.top: parent.top
            anchors.left: timelineIndent.right
            anchors.right: verticalRulerPanelHeader.left

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

                QtObject {
                    id: prv

                    property bool playRegionActivated: false
                }

                onPositionChanged: function(e) {
                    timeline.updateCursorPosition(e.x, e.y)
                    playRegionController.mouseMove(e.x)
                }

                onPressed: function (e) {
                    if (timeline.isMajorSection(e.y)) {
                        playRegionController.mouseDown(e.x)
                        prv.playRegionActivated = true
                    }
                }

                onReleased: function (e) {
                    playRegionController.mouseUp(e.x)
                }

                onClicked: function (e) {
                    if (!prv.playRegionActivated) {
                        playCursorController.seekToX(e.x, true /* triggerPlay */)
                    }
                    prv.playRegionActivated = false
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

                anchors.top: parent.verticalCenter
                anchors.bottom: parent.bottom

                color: "#ABE7FF"
                opacity: 0.3
            }

            PlayRegion {
                id: playRegion

                context: timeline.context
            }

            PlayCursorHead {
                id: head

                anchors.top: parent.top
                anchors.topMargin: 24

                x: playCursorController.positionX

                timelinePressed: timelineMouseArea.pressed

                onSetPlaybackPosition: function(ix) {
                    playCursorController.seekToX(ix)
                }

                onPlayCursorMousePositionChanged: function(ix) {
                    timeline.updateCursorPosition(ix, -1)
                }
            }
        }

        Rectangle {
            id: verticalRulerPanelHeader

            anchors.top: parent.top
            anchors.right: parent.right
            height: timeline.height
            width: tracksModel.isVerticalRulersVisible ? 32 : 0

            color: ui.theme.backgroundSecondaryColor

            SeparatorLine {
                anchors.left: parent.left
                anchors.bottom: parent.bottom

                width: parent.width

                color: ui.theme.strokeColor
            }

            SeparatorLine {
                anchors.top: parent.top
                anchors.left: parent.left

                orientation: Qt.Vertical

                height: parent.height

                color: ui.theme.strokeColor
            }
        }
    }

    CustomCursor {
        id: customCursor
        active: {
            // Don't show custom cursor during playback for sample editing
            if ((content.isNearSample || content.isIsolationMode) && playbackState.isPlaying) {
                return false
            }

            return (content.isIsolationMode || content.isNearSample
                || content.leftTrimContainsMouse || content.rightTrimContainsMouse
                || content.leftTrimPressedButtons || content.rightTrimPressedButtons
                || (root.isSplitMode && root.clipHovered) || (content.isBrush && root.clipHovered))
        }
        source: {
            if (content.isBrush) {
                return smoothShape
            }

            if (content.isNearSample || content.isIsolationMode) {
                return pencilShape
            }

            if (root.isSplitMode) {
                return splitShape
            }

            return content.leftTrimContainsMouse || content.leftTrimPressedButtons ? leftTrimShape : rightTrimShape
        }
        size: content.isIsolationMode || (!content.isBrush && content.isNearSample) ? 36 : 26
    }

    Rectangle {
        id: content
        objectName: "clipsView"
        anchors.leftMargin: 12
        anchors.top: timelineHeader.bottom
        anchors.bottom: parent.bottom
        anchors.left: parent.left
        anchors.right: parent.right

        property bool isBrush: false
        property bool isIsolationMode: false
        property bool isNearSample: false
        property bool leftTrimContainsMouse: false
        property bool rightTrimContainsMouse: false
        property bool leftTrimPressedButtons: false
        property bool rightTrimPressedButtons: false

        GridLines {
            timelineRuler: timeline.ruler
            anchors.fill: parent
        }

        MouseArea {
            id: mainMouseArea
            anchors.fill: parent

            preventStealing: true
            acceptedButtons: Qt.LeftButton | Qt.RightButton
            cursorShape: Qt.IBeamCursor

            hoverEnabled: true

            onWheel: function(wheelEvent) {
                timeline.onWheel(wheelEvent.x, wheelEvent.pixelDelta, wheelEvent.angleDelta)
            }

            onPressed: function(e) {
                if (root.altPressed) {
                    return
                }

                if (e.button === Qt.LeftButton) {

                    tracksModel.startUserInteraction()

                    if (root.clipHeaderHovered) {
                        tracksClipsView.clipStartEditRequested(hoveredClipKey)
                    } else {
                        if (! ((e.modifiers & (Qt.ControlModifier | Qt.ShiftModifier)) || root.isSplitMode)) {
                            playCursorController.seekToX(e.x)
                        }
                        // Hover status will reset after the selection reset
                        let clipWasHovered = root.clipHovered

                        selectionController.onPressed(e.x, e.y)
                        selectionController.resetSelectedClip()
                        clipsSelection.visible = true
                        handleGuideline(e.x, false)

                        if (clipWasHovered && root.isSplitMode) {
                            tracksModel.splitAt(root.hoveredTrackId, timeline.context.positionToTime(splitGuideline.x))
                        }
                    }
                } else if (e.button === Qt.RightButton) {
                    if (tracksHovered) {
                        //! TODO AU4: handle context menu over empty track area
                    } else {
                        canvasContextMenuLoader.show(Qt.point(e.x + timelineIndent.width, e.y + timelineHeader.height), canvasContextMenuModel.items)
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
                    let trackId = tracksViewState.trackAtPosition(e.x, e.y);

                    handleGuideline(e.x, false)
                }
            }
            onReleased: e => {
                if (e.button !== Qt.LeftButton) {
                    return
                }

                if (root.clipHeaderHovered) {
                    tracksClipsView.clipMoveRequested(hoveredClipKey, true)
                    tracksClipsView.stopAutoScroll()
                    tracksClipsView.clipEndEditRequested(hoveredClipKey)
                    root.clipHeaderHovered = false
                } else {
                    if (selectionController.isLeftSelection(e.x)) {
                        playCursorController.seekToX(e.x)
                    }
                    selectionController.onReleased(e.x, e.y)
                    handleGuideline(e.x, true)
                    if (e.modifiers & (Qt.ControlModifier | Qt.ShiftModifier)) {
                        playCursorController.seekToX(timeline.context.selectionStartPosition)
                    }

                    playCursorController.setPlaybackRegion(timeline.context.selectionStartPosition, timeline.context.selectionEndPosition)

                    clipsSelection.visible = false
                }

                tracksModel.endUserInteraction()
            }

            onCanceled: e => {
                console.log("User interaction canceled")
                tracksModel.endUserInteraction()
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

                if (root.isSplitMode) {
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
            anchors.rightMargin: tracksModel.isVerticalRulersVisible ? verticalRulerPanelHeader.width : 0

            view: tracksClipsView

            horizontalScrollbarSize: timeline.context.horizontalScrollbarSize
            startHorizontalScrollPosition: timeline.context.startHorizontalScrollPosition

            verticalScrollbarSize: timeline.context.verticalScrollbarSize
            startVerticalScrollPosition: timeline.context.startVerticalScrollPosition

            StyledListView {
                id: tracksClipsView

                anchors.fill: parent
                clip: false // do not clip so clip handles are visible

                navigation.section: root.navigationSection
                navigation.order: 3

                property bool moveActive: false

                ScrollBar.horizontal: null
                ScrollBar.vertical: null

                property real lockedVerticalScrollPosition
                property bool verticalScrollLocked: tracksViewState.tracksVerticalScrollLocked

                function checkIfAnyTrack(f) {
                    for (let i = 0; i < tracksClipsView.count; i++) {
                        if (f(tracksClipsView.itemAtIndex(i))) {
                            return true
                        }
                    }
                    return false
                }

                signal clipMoveRequested(var clipKey, bool completed)
                signal clipStartEditRequested(var clipKey)
                signal clipEndEditRequested(var clipKey)
                signal startAutoScroll()
                signal stopAutoScroll()

                header: Rectangle {
                    height: 2
                    width: parent.width
                    color: "transparent"
                }

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
                        tracksViewState.changeTracksVerticalOffset(tracksClipsView.contentY)
                        timeline.context.startVerticalScrollPosition = tracksClipsView.contentY
                    }
                }

                onHeightChanged: {
                    timeline.context.onResizeFrameHeight(tracksClipsView.height)
                }

                Connections {
                    target: timeline.context

                    function onViewContentYChangeRequested(delta) {
                        let headerHeight = tracksClipsView.headerItem ? tracksClipsView.headerItem.height : 0
                        let totalContentHeight = tracksModel.totalTracksHeight + tracksViewState.tracksVerticalScrollPadding
                        let canMove = totalContentHeight > tracksClipsView.height
                        if (!canMove) {
                            return
                        }

                        let contentYOffset = tracksClipsView.contentY + delta

                        let maxContentY = totalContentHeight - tracksClipsView.height
                        maxContentY = Math.max(maxContentY, tracksClipsView.contentY)
                        contentYOffset = Math.max(Math.min(contentYOffset, maxContentY), -headerHeight)

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
                    isTrackFocused: model.isTrackFocused
                    isMultiSelectionActive: model.isMultiSelectionActive
                    isTrackAudible: model.isTrackAudible
                    moveActive: tracksClipsView.moveActive
                    altPressed: root.altPressed
                    ctrlPressed: root.ctrlPressed
                    selectionEditInProgress: selectionController.selectionEditInProgress
                    selectionInProgress: selectionController.selectionInProgress
                    onHoverChanged: function() {
                        root.clipHovered = tracksClipsView.checkIfAnyTrack(function(trackItem) {
                            return trackItem && trackItem.hover
                        })
                    }

                    trackIdx: model.index
                    navigationSection: root.navigationSection
                    navigationPanel: tracksClipsView.navigation

                    onTrackItemMousePositionChanged: function(xWithinTrack, yWithinTrack, clipKey) {
                        let xGlobalPosition = xWithinTrack
                        let yGlobalPosition = y + yWithinTrack - tracksClipsView.contentY

                        timeline.updateCursorPosition(xGlobalPosition, yGlobalPosition)

                        root.hoveredTrackId = trackId
                        root.hoveredClipKey = clipKey
                        root.hoveredTrackHeight = tracksViewState.trackHeight(trackId)
                        root.hoveredTrackVerticalPosition = tracksViewState.trackVerticalPosition(trackId)

                        root.splitGuidelinePosition = xWithinTrack
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
                        selectionContextMenuLoader.show(Qt.point(x + canvasIndent.width, y + timelineHeader.height), selectionContextMenuModel.items)
                    }

                    onSelectionDraged: function(x1, x2, completed) {
                        selectionController.onSelectionDraged(x1, x2, completed)
                    }

                    onSeekToX: function(x) {
                        playCursorController.seekToX(x)
                    }

                    onInsureVerticallyVisible: function(clipTop, clipBottom) {
                        var delta = calculateVerticalScrollDelta(tracksViewState.tracksVerticalOffset, tracksViewState.tracksVerticalOffset + content.height, clipTop, clipBottom)
                        if (tracksViewState.tracksVerticalOffset + delta < 0) {
                            tracksViewState.changeTracksVerticalOffset(0)
                        } else {
                            tracksViewState.changeTracksVerticalOffset(tracksViewState.tracksVerticalOffset + delta)
                        }
                    }

                    onInteractionStarted: {
                        tracksViewState.requestVerticalScrollLock()
                    }

                    onInteractionEnded: {
                        tracksViewState.requestVerticalScrollUnlock()
                    }

                    onIsBrushChanged: function() {
                        content.isBrush = tracksClipsView.checkIfAnyTrack(function(track) {
                            return track && track.isBrush
                        })
                    }

                    onIsIsolationModeChanged: function() {
                        content.isIsolationMode = tracksClipsView.checkIfAnyTrack(function(track){
                            return track && track.isIsolationMode
                        })
                    }

                    onIsNearSampleChanged: function() {
                        content.isNearSample = tracksClipsView.checkIfAnyTrack(function(track){
                            return track &&track.isNearSample
                        })
                    }

                    onLeftTrimContainsMouseChanged: function() {
                        content.leftTrimContainsMouse = tracksClipsView.checkIfAnyTrack(function(track){
                            return track && track.leftTrimContainsMouse
                        })
                    }

                    onRightTrimContainsMouseChanged: function() {
                        content.rightTrimContainsMouse = tracksClipsView.checkIfAnyTrack(function(track){
                            return track && track.rightTrimContainsMouse
                        })
                    }

                    onLeftTrimPressedButtonsChanged: function() {
                        content.leftTrimPressedButtons = tracksClipsView.checkIfAnyTrack(function(track){
                            return track && track.leftTrimPressedButtons
                        })
                    }

                    onRightTrimPressedButtonsChanged: function() {
                        content.rightTrimPressedButtons = tracksClipsView.checkIfAnyTrack(function(track){
                            return track && track.rightTrimPressedButtons
                        })
                    }

                    onTriggerClipGuideline: function(time, completed) {
                        clipGuideline.x = timeline.context.timeToPosition(time)
                        root.guidelineActive = clipGuideline.x >= 0 && !completed
                    }

                    onHandleTimeGuideline: function(x) {
                        root.handleGuideline(x)
                    }

                    function calculateVerticalScrollDelta(viewTop, viewBottom, clipTop, clipBottom, padding = 10) {
                        // clip fully visible
                        if (clipTop >= viewTop && clipBottom <= viewBottom) {
                            return 0
                        }

                        // clip is above the view —> scroll up
                        if (clipTop < viewTop) {
                            return clipTop - (viewTop + padding)
                        }

                        // clip is below the view —> scroll down
                        if (clipBottom > viewBottom) {
                            return clipBottom - (viewBottom - padding)
                        }

                        return 0;
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

        PlayCursorLine {
            id: playCursor

            anchors.top: tracksClipsViewArea.top
            anchors.bottom: parent.bottom

            x: playCursorController.positionX

            timelinePressed: timelineMouseArea.pressed
        }

        Rectangle {
            id: clipGuideline

            anchors.top: content.top
            anchors.bottom: content.bottom

            width: 1

            color: tracksViewState.snapEnabled ? "#00E5FF" : "#FFF200"

            visible: root.guidelineActive
        }

        Rectangle {
            id: splitGuideline

            x: root.guidelineActive ? clipGuideline.x : splitGuidelinePosition
            y: hoveredTrackVerticalPosition
            width: 1
            height: hoveredTrackHeight

            color: "#0121C0"

            visible: root.isSplitMode && root.clipHovered
        }

        VerticalRulersPanel {
            id: verticalRulers

            model: tracksModel
            context: timeline.context

            height: parent.height
            width: verticalRulerPanelHeader.width
            anchors.right: parent.right
            anchors.bottom: parent.bottom

            visible: tracksModel.isVerticalRulersVisible
        }
    }

    DropArea {
        anchors.fill: parent
        onDropped: (drop) => {
            let urls = drop.urls.concat([]); // Forces conversion to a compatible array
            tracksModel.handleDroppedFiles(urls);

            drop.acceptProposedAction()
        }
    }

    function handleGuideline(x, completed) {

        let time = timeline.context.positionToTime(x)
        time = timeline.context.applyDetectedSnap(time)
        let guidelineTimePos = timeline.context.findGuideline(time)

        if (guidelineTimePos !== -1) {
            clipGuideline.x = timeline.context.timeToPosition(guidelineTimePos)

            root.guidelineActive = clipGuideline.x >= 0 ? !completed : false
        } else {
            root.guidelineActive = false
        }
    }
}
