import QtQuick
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene
import Audacity.Project
import Audacity.Playback

Rectangle {
    id: root

    property var navPanels: null

    property bool itemHovered: false
    property bool itemHeaderHovered: false
    property var hoveredItemKey: null

    property var hoveredTrackId: null
    property double hoveredTrackVerticalPosition
    property double hoveredTrackHeight
    property bool tracksHovered: false

    property bool guidelineVisible: false
    property double guidelinePos: -1

    property alias altPressed: tracksViewState.altPressed
    property alias ctrlPressed: tracksViewState.ctrlPressed
    property alias isSplitMode: splitToolController.active

    color: ui.theme.backgroundPrimaryColor

    clip: true

    enum State {
        Idle,
        DraggingItem
    }

    property int interactionState: TracksItemsView.State.Idle

    MouseHelper {
        id: mouseHelper
    }

    QtObject {
        id: prv

        property bool playRegionActivated: false

        function cancelClipDragEdit() {
            if (mainMouseArea.pressed) {
                // This will lead to a cancel signal on `mainMouseArea` that will call back into this function,
                // but this time in released state.
                mouseHelper.callUngrabMouseOnItem(mainMouseArea)
                return
            }
            if (root.hoveredClipKey) {
                tracksClipsView.cancelClipDragEditRequested(root.hoveredClipKey)
            }
        }
    }

    PlaybackStateModel {
        id: playbackState
    }

    ViewTracksListModel {
        id: tracksModel

        onTotalTracksHeightChanged: {
            timeline.context.onResizeFrameContentHeight(tracksModel.totalTracksHeight)
        }

        onEscapePressed: {
            prv.cancelClipDragEdit()
        }
    }

    ProjectPropertiesModel {
        id: project

        onCaptureThumbnail: function captureThumbnail(thumbnailUrl) {
            // hide playCursor for the time grabbing image
            playCursor.visible = false
            content.grabToImage(function (result) {
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

        onHandleMenuItem: function (itemId) {
            selectionContextMenuModel.handleMenuItem(itemId)
        }
    }

    CanvasContextMenuModel {
        id: canvasContextMenuModel
    }

    ContextMenuLoader {
        id: canvasContextMenuLoader

        onHandleMenuItem: function (itemId) {
            canvasContextMenuModel.handleMenuItem(itemId)
        }
    }

    //! NOTE Sync with TracksPanel
    TracksViewStateModel {
        id: tracksViewState
        onTracksVerticalOffsetChanged: {
            tracksItemsView.contentY = tracksViewState.tracksVerticalOffset
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

    SplitToolController {
        id: splitToolController
        context: timeline.context

        clipHovered: root.itemHovered && !root.itemHeaderHovered
        hoveredTrack: root.hoveredTrackId
    }

    SelectionViewController {
        id: selectionController
        context: timeline.context
    }

    Component.onCompleted: {
        //! NOTE Models depend on geometry, so let's create a page first and then initialize the models
        Qt.callLater(root.init)

        selectionController.load()
        selectionContextMenuModel.load()
        canvasContextMenuModel.load()

        splitToolController.init(root)
    }

    function init() {
        timeline.init()
        playRegionController.init()
        playCursorController.init()
        playPositionActionController.init()
        tracksViewState.init()
        project.init();

        //! NOTE Loading tracks, or rather clips, is the most havy operation.
        // Let's make sure that everything is loaded and initialized before this,
        // to avoid double loading at the beginning, when some parameters are initialized.
        Qt.callLater(tracksModel.load);

        //! NOTE setting verticalY has to be done after tracks are loaded,
        // otherwise project always starts at the very top
        Qt.callLater(() => tracksItemsView.contentY = tracksViewState.tracksVerticalOffset)
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

        MouseArea {
            id: timelineMouseArea
            anchors.fill: parent
            hoverEnabled: true

            property bool playRegionActivated: false

            function convertToTimelinePosition(e) {
                let position = mapToItem(timeline, Qt.point(e.x, e.y))
                position.x = Math.max(0, position.x)
                position.y = Math.max(0, position.y)
                return position
            }

            onPositionChanged: function (e) {
                let position = convertToTimelinePosition(e)
                timeline.updateCursorPosition(position.x, position.y)
                playRegionController.updatePosition(position.x)
            }

            onPressed: function (e) {
                let position = convertToTimelinePosition(e)
                if (timeline.isMajorSection(position.y)) {
                    playRegionController.startInteraction(position.x, root.ctrlPressed)
                    playRegionActivated = true
                }
            }

            onReleased: function (e) {
                let position = convertToTimelinePosition(e)
                playRegionController.finishInteraction(position.x)
            }

            onClicked: function (e) {
                let position = convertToTimelinePosition(e)
                if (!playRegionActivated) {
                    playCursorController.seekToX(position.x, timeline.context.playbackOnRulerClickEnabled)
                }
                playRegionActivated = false
            }

            Connections {
                target: root

                function onCtrlPressedChanged() {
                    if (!root.ctrlPressed) {
                        playRegionController.finishInteraction(timelineMouseArea.mouseX)
                    }
                }
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
                tracksViewState.setMouseY(Math.max(0, Math.min(y, mainMouseArea.height)))
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

                x: timeline.context.singleItemSelected ? timeline.context.selectedItemStartPosition : timeline.context.selectionStartPosition
                width: timeline.context.singleItemSelected ? timeline.context.selectedItemEndPosition - x : timeline.context.selectionEndPosition - x

                anchors.top: parent.verticalCenter
                anchors.bottom: parent.bottom

                color: "#ABE7FF"
                opacity: 0.3
            }

            PlayCursorHead {
                id: head

                anchors.top: parent.top
                anchors.topMargin: 24

                x: playCursorController.positionX

                timelinePressed: timelineMouseArea.pressed

                onSetPlaybackPosition: function (ix) {
                    playCursorController.seekToX(ix)
                }

                onPlayCursorMousePositionChanged: function (ix) {
                    timeline.updateCursorPosition(ix, 0)
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

        readonly property string pencilShape: ":/images/customCursorShapes/Pencil.png"
        readonly property string smoothShape: ":/images/customCursorShapes/Smooth.png"
        readonly property string leftTrimShape: ":/images/customCursorShapes/ClipTrimLeft.png"
        readonly property string rightTrimShape: ":/images/customCursorShapes/ClipTrimRight.png"

        active: {
            // Don't show custom cursor during playback for sample editing
            if ((content.isNearSample || content.isIsolationMode) && playbackState.isPlaying) {
                return false
            }

            return (content.isIsolationMode || content.isNearSample || content.leftTrimContainsMouse || content.rightTrimContainsMouse || content.leftTrimPressedButtons || content.rightTrimPressedButtons || (content.isBrush && root.itemHovered))
        }
        source: {
            if (content.isBrush) {
                return smoothShape
            }

            if (content.isNearSample || content.isIsolationMode) {
                return pencilShape
            }

            return content.leftTrimContainsMouse || content.leftTrimPressedButtons ? leftTrimShape : rightTrimShape
        }
        size: content.isIsolationMode || (!content.isBrush && content.isNearSample) ? 36 : 26
    }

    Rectangle {
        id: content
        objectName: "ItemsView"
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

            onWheel: function (wheelEvent) {
                timeline.onWheel(wheelEvent.x, wheelEvent.pixelDelta, wheelEvent.angleDelta)
            }

            onPressed: function (e) {
                if (root.altPressed) {
                    return
                }

                if (e.button === Qt.LeftButton) {
                    if (root.itemHeaderHovered) {
                        tracksItemsView.itemStartEditRequested(hoveredItemKey)
                        root.interactionState = TracksItemsView.State.DraggingItem
                    } else {
                        if (!((e.modifiers & (Qt.ControlModifier | Qt.ShiftModifier)) || root.isSplitMode)) {
                            playCursorController.seekToX(e.x)
                        }

                        if (!splitToolController.active) {
                            selectionController.onPressed(e.x, e.y)
                            selectionController.resetSelectedItems()
                            itemsSelection.visible = true
                        }
                        handleGuideline(e.x, false)

                        splitToolController.mouseDown(e.x)
                    }
                } else if (e.button === Qt.RightButton) {
                    if (tracksHovered)
                    //! TODO AU4: handle context menu over empty track area
                    {} else {
                        canvasContextMenuLoader.show(Qt.point(e.x + timelineIndent.width, e.y + timelineHeader.height), canvasContextMenuModel.items)
                    }
                }
            }

            onPositionChanged: function (e) {
                timeline.updateCursorPosition(e.x, e.y)
                splitToolController.mouseMove(e.x)

                if (root.interactionState === TracksItemsView.State.DraggingItem) {
                    tracksItemsView.itemMoveRequested(hoveredItemKey, false)
                    tracksItemsView.startAutoScroll()
                } else {
                    selectionController.onPositionChanged(e.x, e.y)
                    let trackId = tracksViewState.trackAtPosition(e.x, e.y)

                    handleGuideline(e.x, false)
                }
            }

            onReleased: e => {
                if (e.button !== Qt.LeftButton) {
                    return
                }

                if (root.interactionState === TracksItemsView.State.DraggingItem) {
                    root.interactionState = TracksItemsView.State.Idle
                    tracksItemsView.itemMoveRequested(hoveredItemKey, true)
                    tracksItemsView.stopAutoScroll()
                    tracksItemsView.itemEndEditRequested(hoveredItemKey)
                } else {
                    splitToolController.mouseUp(e.x)

                    if (selectionController.isLeftSelection(e.x)) {
                        playCursorController.seekToX(e.x)
                    }
                    if (!splitToolController.active) {
                        selectionController.onReleased(e.x, e.y)
                        itemsSelection.visible = false
                    }
                    handleGuideline(e.x, true)
                    if (e.modifiers & (Qt.ControlModifier | Qt.ShiftModifier)) {
                        playCursorController.seekToX(timeline.context.selectionStartPosition)
                    }

                    playCursorController.setPlaybackRegion(timeline.context.selectionStartPosition, timeline.context.selectionEndPosition)
                }
            }

            onCanceled: e => {
                root.interactionState = TracksItemsView.State.Idle
                prv.cancelClipDragEdit()
            }

            onClicked: e => {
                if (e.button !== Qt.LeftButton) {
                    return
                }

                if (!root.itemHovered) {
                    selectionController.resetSelectedItems()
                }
            }

            onDoubleClicked: e => {
                if (e.button !== Qt.LeftButton) {
                    return
                }

                if (root.isSplitMode) {
                    return
                }

                if (root.itemHovered) {
                    selectionController.selectItemData(root.hoveredItemKey)
                    playCursorController.setPlaybackRegion(timeline.context.selectedItemStartPosition, timeline.context.selectedItemEndPosition)
                } else {
                    selectionController.selectTrackAudioData(e.y)
                    playCursorController.setPlaybackRegion(timeline.context.selectedItemStartPosition, timeline.context.selectedItemEndPosition)
                }
                itemsSelection.visible = false
            }
        }

        StyledViewScrollAndZoomArea {
            id: tracksItemsViewArea

            anchors.fill: parent
            anchors.rightMargin: tracksModel.isVerticalRulersVisible ? verticalRulerPanelHeader.width : 0

            view: tracksItemsView

            horizontalScrollbarSize: timeline.context.horizontalScrollbarSize
            startHorizontalScrollPosition: timeline.context.startHorizontalScrollPosition

            verticalScrollbarSize: timeline.context.verticalScrollbarSize
            startVerticalScrollPosition: timeline.context.startVerticalScrollPosition

            StyledListView {
                id: tracksItemsView

                anchors.fill: parent
                clip: false // do not clip so clip handles are visible

                property bool moveActive: false

                ScrollBar.horizontal: null
                ScrollBar.vertical: null

                property real lockedVerticalScrollPosition
                property bool verticalScrollLocked: tracksViewState.tracksVerticalScrollLocked

                function checkIfAnyTrack(f) {
                    for (let i = 0; i < tracksItemsView.count; i++) {
                        let trackLoader = tracksItemsView.itemAtIndex(i)
                        if (trackLoader && trackLoader.item) {
                            if (f(trackLoader.item)) {
                                return true
                            }
                        }
                    }
                    return false
                }

                signal itemMoveRequested(var itemKey, bool completed)
                signal itemStartEditRequested(var itemKey)
                signal itemEndEditRequested(var itemKey)
                signal cancelItemDragEditRequested(var itemKey)
                signal startAutoScroll
                signal stopAutoScroll

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
                    } else {
                        tracksViewState.changeTracksVerticalOffset(tracksItemsView.contentY)
                        timeline.context.startVerticalScrollPosition = tracksItemsView.contentY
                    }
                }

                onHeightChanged: {
                    timeline.context.onResizeFrameHeight(tracksItemsView.height)
                }

                Connections {
                    target: timeline.context

                    function onViewContentYChangeRequested(delta) {
                        let headerHeight = tracksItemsView.headerItem ? tracksItemsView.headerItem.height : 0
                        let totalContentHeight = tracksModel.totalTracksHeight + tracksViewState.tracksVerticalScrollPadding
                        let canMove = totalContentHeight > tracksItemsView.height
                        if (!canMove) {
                            return
                        }

                        let contentYOffset = tracksItemsView.contentY + delta

                        let maxContentY = totalContentHeight - tracksItemsView.height
                        maxContentY = Math.max(maxContentY, tracksItemsView.contentY)
                        contentYOffset = Math.max(Math.min(contentYOffset, maxContentY), -headerHeight)

                        tracksItemsView.contentY = contentYOffset
                    }
                }

                interactive: false

                model: tracksModel

                delegate: Loader {
                    id: trackItemLoader

                    property var itemData: model
                    property int index: model.index

                    width: tracksItemsView.width

                    sourceComponent: trackType === TrackType.LABEL ? trackLabelsContainer : trackClipsContainerComp

                    onLoaded: {
                        trackItemLoader.item.init()
                    }

                    Component {
                        id: trackClipsContainerComp

                        TrackClipsContainer {
                            property var itemData: trackItemLoader.itemData
                            property int index: trackItemLoader.index

                            width: trackItemLoader.width

                            context: timeline.context

                            container: tracksItemsView
                            canvas: content

                            trackId: itemData.trackId

                            isDataSelected: itemData.isDataSelected
                            isTrackSelected: itemData.isTrackSelected
                            isTrackFocused: itemData.isTrackFocused
                            isMultiSelectionActive: itemData.isMultiSelectionActive
                            isTrackAudible: itemData.isTrackAudible
                            isLinear: itemData.isLinear
                            dbRange: itemData.dbRange
                            isWaveformViewVisible: itemData.isWaveformViewVisible
                            isSpectrogramViewVisible: itemData.isSpectrogramViewVisible

                            moveActive: tracksItemsView.moveActive

                            altPressed: root.altPressed
                            ctrlPressed: root.ctrlPressed

                            selectionEditInProgress: selectionController.selectionEditInProgress
                            selectionInProgress: selectionController.selectionInProgress
                            onHoverChanged: function () {
                                root.itemHovered = tracksItemsView.checkIfAnyTrack(function (trackItem) {
                                    return trackItem && trackItem.hover
                                })
                            }

                            navigationPanel: navPanels && navPanels[index] ? navPanels[index] : null

                            onTrackItemMousePositionChanged: function (xWithinTrack, yWithinTrack, itemKey) {
                                let xGlobalPosition = xWithinTrack
                                let yGlobalPosition = y + yWithinTrack - tracksItemsView.contentY

                                timeline.updateCursorPosition(xGlobalPosition, yGlobalPosition)

                                root.hoveredTrackId = trackId
                                root.hoveredItemKey = itemKey
                                root.hoveredTrackHeight = tracksViewState.trackHeight(trackId)
                                root.hoveredTrackVerticalPosition = tracksViewState.trackVerticalPosition(trackId)

                                splitToolController.mouseMove(xWithinTrack)
                            }

                            onSetHoveredItemKey: function (itemKey) {
                                root.hoveredItemKey = itemKey
                            }

                            onItemHeaderHoveredChanged: function (val) {
                                root.itemHeaderHovered = val
                            }

                            onItemSelectedRequested: {
                                selectionController.resetDataSelection()
                                itemsSelection.visible = false
                            }

                            onSelectionResetRequested: {
                                selectionController.resetDataSelection()
                            }

                            onUpdateMoveActive: function (completed) {
                                if (tracksItemsView.moveActive !== completed) {
                                    return
                                }
                                tracksItemsView.moveActive = !completed
                            }

                            onRequestSelectionContextMenu: function (x, y) {
                                selectionContextMenuLoader.show(Qt.point(x + canvasIndent.width, y + timelineHeader.height), selectionContextMenuModel.items)
                            }

                            onSelectionDraged: function (x1, x2, completed) {
                                selectionController.onSelectionDraged(x1, x2, completed)
                            }

                            onSeekToX: function (x) {
                                playCursorController.seekToX(x)
                            }

                            onInsureVerticallyVisible: function (clipTop, clipBottom) {
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

                            onItemDragEditCanceled: {
                                root.hoveredClipKey = null
                                root.clipHeaderHovered = false
                                tracksClipsView.moveActive = false
                                timeline.context.updateSelectedClipTime()
                            }

                            onIsBrushChanged: function () {
                                content.isBrush = tracksItemsView.checkIfAnyTrack(function (track) {
                                    return track && track.isBrush
                                })
                            }

                            onIsIsolationModeChanged: function () {
                                content.isIsolationMode = tracksItemsView.checkIfAnyTrack(function (track) {
                                    return track && track.isIsolationMode
                                })
                            }

                            onIsNearSampleChanged: function () {
                                content.isNearSample = tracksItemsView.checkIfAnyTrack(function (track) {
                                    return track && track.isNearSample
                                })
                            }

                            onLeftTrimContainsMouseChanged: function () {
                                content.leftTrimContainsMouse = tracksItemsView.checkIfAnyTrack(function (track) {
                                    return track && track.leftTrimContainsMouse
                                })
                            }

                            onRightTrimContainsMouseChanged: function () {
                                content.rightTrimContainsMouse = tracksItemsView.checkIfAnyTrack(function (track) {
                                    return track && track.rightTrimContainsMouse
                                })
                            }

                            onLeftTrimPressedButtonsChanged: function () {
                                content.leftTrimPressedButtons = tracksItemsView.checkIfAnyTrack(function (track) {
                                    return track && track.leftTrimPressedButtons
                                })
                            }

                            onRightTrimPressedButtonsChanged: function () {
                                content.rightTrimPressedButtons = tracksItemsView.checkIfAnyTrack(function (track) {
                                    return track && track.rightTrimPressedButtons
                                })
                            }

                            onTriggerClipGuideline: function (time, completed) {
                                root.guidelinePos = timeline.context.timeToPosition(time)
                                root.guidelineVisible = root.guidelinePos >= 0 && !completed
                            }

                            onHandleTimeGuideline: function (x) {
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

                                return 0
                            }
                        }
                    }

                    Component {
                        id: trackLabelsContainer

                        TrackLabelsContainer {
                            property var itemData: trackItemLoader.itemData
                            property int index: trackItemLoader.index

                            width: trackItemLoader.width

                            context: timeline.context
                            container: tracksItemsView
                            canvas: content

                            trackId: itemData.trackId
                            isDataSelected: itemData.isDataSelected
                            isTrackSelected: itemData.isTrackSelected
                            isTrackFocused: itemData.isTrackFocused
                            isMultiSelectionActive: itemData.isMultiSelectionActive
                            isTrackAudible: itemData.isTrackAudible

                            moveActive: tracksItemsView.moveActive

                            altPressed: root.altPressed
                            ctrlPressed: root.ctrlPressed

                            selectionEditInProgress: selectionController.selectionEditInProgress
                            selectionInProgress: selectionController.selectionInProgress

                            navigationPanel: navPanels && navPanels[index] ? navPanels[index] : null

                            onHoverChanged: function () {
                                root.itemHovered = tracksItemsView.checkIfAnyTrack(function (trackItem) {
                                    return trackItem && trackItem.hover
                                })
                            }

                            onTrackItemMousePositionChanged: function (xWithinTrack, yWithinTrack, itemKey) {
                                let xGlobalPosition = xWithinTrack
                                let yGlobalPosition = y + yWithinTrack - tracksItemsView.contentY

                                timeline.updateCursorPosition(xGlobalPosition, yGlobalPosition)

                                root.hoveredTrackId = trackId
                                root.hoveredItemKey = itemKey
                                root.hoveredTrackHeight = tracksViewState.trackHeight(trackId)
                                root.hoveredTrackVerticalPosition = tracksViewState.trackVerticalPosition(trackId)

                                splitToolController.mouseMove(xWithinTrack)
                            }

                            onSetHoveredItemKey: function (itemKey) {
                                root.hoveredItemKey = itemKey
                            }

                            onItemHeaderHoveredChanged: function (val) {
                                root.itemHeaderHovered = val
                            }

                            onInteractionStarted: {
                                tracksViewState.requestVerticalScrollLock()
                            }

                            onInteractionEnded: {
                                tracksViewState.requestVerticalScrollUnlock()
                            }

                            onSeekToX: function (x) {
                                playCursorController.seekToX(x)
                            }

                            onSelectionDraged: function (x1, x2, completed) {
                                selectionController.onSelectionDraged(x1, x2, completed)
                            }

                            onRequestSelectionContextMenu: function (x, y) {
                                selectionContextMenuLoader.show(Qt.point(x + canvasIndent.width, y + timelineHeader.height), selectionContextMenuModel.items)
                            }

                            onItemSelectedRequested: {
                                selectionController.resetDataSelection()
                                itemsSelection.visible = false
                            }

                            onSelectionResetRequested: {
                                selectionController.resetDataSelection()
                            }

                            onUpdateMoveActive: function (completed) {
                                if (tracksItemsView.moveActive !== completed) {
                                    return
                                }
                                tracksItemsView.moveActive = !completed
                            }
                        }
                    }
                }
            }

            onPinchToZoom: function (scale, pos) {
                timeline.context.pinchToZoom(scale, pos)
            }

            onScrollHorizontal: function (newPos) {
                timeline.context.scrollHorizontal(newPos)
            }

            onScrollVertical: function (newPos) {
                timeline.context.scrollVertical(newPos)
            }
        }

        Rectangle {
            id: itemsSelection

            anchors.top: parent.top
            anchors.bottom: parent.bottom
            color: "#ABE7FF"
            opacity: 0.05
            visible: false

            x: Math.max(timeline.context.selectionStartPosition, 0.0)
            width: timeline.context.selectionEndPosition - x
        }

        PlaybackPositionTimer {}

        PlayCursorLine {
            id: playCursor

            anchors.top: tracksItemsViewArea.top
            anchors.bottom: parent.bottom

            x: playCursorController.positionX
        }

        Rectangle {
            id: clipGuideline

            anchors.top: content.top
            anchors.bottom: content.bottom

            width: 1
            x: playRegionController.guidelineVisible ? playRegionController.guidelinePosition : (root.guidelineVisible ? root.guidelinePos : -1)

            color: tracksViewState.snapEnabled ? "#00E5FF" : "#FFF200"

            visible: root.guidelineVisible || playRegionController.guidelineVisible
        }

        Rectangle {
            id: splitGuideline

            x: splitToolController.guidelinePosition
            y: splitToolController.singleTrack ? hoveredTrackVerticalPosition : 0

            width: 1
            height: splitToolController.singleTrack ? hoveredTrackHeight : content.height

            color: "#0121C0"

            visible: splitToolController.guidelineVisible
        }

        VerticalRulersPanel {
            id: verticalRulers

            model: tracksModel
            context: timeline.context

            height: parent.height
            width: verticalRulerPanelHeader.width
            anchors.right: parent.right
            anchors.bottom: parent.bottom
        }
    }

    DropArea {
        anchors.fill: parent
        onDropped: drop => {
            let urls = drop.urls.concat([]);
            // Forces conversion to a compatible array
            tracksModel.handleDroppedFiles(urls)

            drop.acceptProposedAction()
        }
    }

    function handleGuideline(x, completed) {
        let time = timeline.context.positionToTime(x)
        time = timeline.context.applyDetectedSnap(time)
        let guidelineTimePos = timeline.context.findGuideline(time)

        if (guidelineTimePos !== -1) {
            root.guidelinePos = timeline.context.timeToPosition(guidelineTimePos)

            root.guidelineVisible = root.guidelinePos >= 0 ? !completed : false
        } else {
            root.guidelineVisible = false
        }
    }
}
