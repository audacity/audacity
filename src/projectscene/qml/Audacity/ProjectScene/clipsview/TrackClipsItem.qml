import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Item {

    id: root

    property NavigationSection navigationSection: null
    property NavigationPanel navigationPanel: null

    property int trackIdx: null
    property alias trackId: clipsModel.trackId
    property alias context: clipsModel.context
    property var canvas: null
    property var container: null

    property bool isDataSelected: false
    property bool isTrackSelected: false
    property bool isTrackFocused: false
    property bool isMultiSelectionActive: false
    property bool isTrackAudible: true
    property bool isStereo: clipsModel.isStereo
    property bool moveActive: false
    property bool altPressed: false
    property bool ctrlPressed: false
    property alias isNearSample: clipsContainer.isNearSample
    property alias isBrush: clipsContainer.isBrush
    property alias isIsolationMode: clipsContainer.isIsolationMode
    property alias leftTrimContainsMouse: clipsContainer.leftTrimContainsMouse
    property alias rightTrimContainsMouse: clipsContainer.rightTrimContainsMouse
    property alias leftTrimPressedButtons: clipsContainer.leftTrimPressedButtons
    property alias rightTrimPressedButtons: clipsContainer.rightTrimPressedButtons
    property bool selectionEditInProgress: false
    property bool selectionInProgress: false
    property bool hover: false

    signal interactionStarted()
    signal interactionEnded()
    // mouse position event is not propagated on overlapping mouse areas
    // so we are handling it manually
    signal trackItemMousePositionChanged(real x, real y, var clipKey)
    signal setHoveredClipKey(var clipKey)
    signal clipSelectedRequested()
    signal selectionResetRequested()
    signal updateMoveActive(bool completed)
    signal requestSelectionContextMenu(real x, real y)

    signal selectionDraged(var x1, var x2, var completed)
    signal seekToX(var x)
    signal insureVerticallyVisible(var top, var bottom)

    signal clipHeaderHoveredChanged(bool val)

    signal handleTimeGuideline(real x, bool completed)
    signal triggerClipGuideline(real x, bool completed)

    height: trackViewState.trackHeight

    ClipsListModel {
        id: clipsModel
    }

    TrackViewStateModel {
        id: trackViewState
        trackId: root.trackId
    }

    Component.onCompleted: {
        trackViewState.init()
        clipsModel.init()
    }

    function changeClipTitle(index, newTitle) {
        clipsModel.changeClipTitle(index, newTitle)
        clipsModel.resetSelectedClip()
    }

    onCtrlPressedChanged: {
        if (!root.ctrlPressed) {
            clipsContainer.mapToAllClips({x: 0, y: 0}, function(clipItem, mouseEvent) {
                clipItem.isIsolationMode = false
            })
        }
    }

    Item {
        id: clipsContainer
        anchors.fill: parent
        anchors.bottomMargin: sep.height
        z: 1

        property double targetHeightRatio: 0.5
        readonly property int minChannelHeight: 20
        readonly property int yMinValue: Math.min(root.height / 2, minChannelHeight)
        readonly property int yMaxValue: Math.max(root.height / 2, root.height - minChannelHeight);

        property bool isNearSample: false
        property bool multiSampleEdit: false
        property bool isBrush: false
        property bool isIsolationMode: false
        property int currentChannel: 0
        property bool leftTrimContainsMouse: false
        property bool rightTrimContainsMouse: false
        property bool leftTrimPressedButtons: false
        property bool rightTrimPressedButtons: false

        function mapToAllClips(e, f) {
            for (let i = 0; i < repeator.count; i++) {
                let clipLoader = repeator.itemAt(i)
                if (clipLoader && clipLoader.item) {
                    let clipPos = clipLoader.mapFromItem(this, e.x, e.y)
                    f(clipLoader.item, {button: e.button, modifiers: e.modifiers, x: clipPos.x, y: clipPos.y})
                }
            }
        }

        function checkIfAnyClip(f) {
            for (let i = 0; i < repeator.count; i++) {
                let clipLoader = repeator.itemAt(i)
                if (clipLoader && clipLoader.item) {
                    if (f(clipLoader.item)) {
                        return true
                    }
                }
            }
            return false
        }

        function updateChannelHeightRatio(position) {
            const newY = Math.min(Math.max(position, yMinValue), yMaxValue)
            trackViewState.changeChannelHeightRatio(newY / height)
        }

        function resetTargetHeightRatio() {
            targetHeightRatio = trackViewState.channelHeightRatio
        }

        onHeightChanged: {
            updateChannelHeightRatio(clipsContainer.targetHeightRatio * clipsContainer.height)
        }

        MouseArea {
            id: clipsContainerMouseArea
            propagateComposedEvents: true
            hoverEnabled: true
            pressAndHoldInterval: 0
            enabled: !root.selectionInProgress && (clipsContainer.isNearSample || clipsContainer.isBrush)
            cursorShape: root.selectionEditInProgress ? Qt.SizeHorCursor : Qt.IBeamCursor 

            anchors.fill: parent

            onDoubleClicked: function(e) {
                e.accepted = true
            }

            onPressAndHold: function(e) {
                if (clipsContainer.isNearSample || root.altPressed) {

                    if (root.ctrlPressed) {
                        clipsContainer.isIsolationMode = true
                    }
                    else if (!root.altPressed) {
                        clipsContainer.multiSampleEdit = true
                    }

                    clipsContainer.mapToAllClips(e, function(clipItem, mouseEvent) {
                        clipItem.mousePressAndHold(mouseEvent.x, mouseEvent.y)
                        clipItem.setLastSample(mouseEvent.x, mouseEvent.y)

                        if (root.ctrlPressed) {
                            clipItem.isIsolationMode = true
                        }
                        else if (!root.altPressed) {
                            clipItem.multiSampleEdit = true
                        }
                        clipItem.currentChannel = clipsContainer.currentChannel
                    })

                    clipsContainerMouseArea.hoverEnabled = true
                    e.accepted = false
                }
            }

            onReleased: function(e) {
                clipsContainer.multiSampleEdit = false
                clipsContainer.isIsolationMode = false

                clipsContainer.mapToAllClips(e, function(clipItem, mouseEvent) {
                    clipItem.multiSampleEdit = false
                    clipItem.isIsolationMode = false
                    clipItem.mouseReleased(mouseEvent.x, mouseEvent.y)
                })
            }

            onPositionChanged: function(e) {
                clipsContainer.mapToAllClips(e, function(clipItem, mouseEvent) {
                    clipItem.mousePositionChanged(mouseEvent.x, mouseEvent.y)
                    clipItem.setLastSample(mouseEvent.x, mouseEvent.y)
                })
            }

            onContainsMouseChanged: function() {
                clipsContainer.mapToAllClips({x: mouseX, y: mouseY}, function(clipItem, mouseEvent) {
                    clipItem.containsMouseChanged(containsMouse)
                })
            }
        }

        Repeater {
            id: repeator

            model: clipsModel

            delegate: Loader {
                property QtObject clipItem: model.item
                property int index: model.index

                height: parent.height
                width: Math.max(3, clipItem.width)
                x: clipItem.x

                asynchronous: true

                sourceComponent: {
                    if ((clipItem.x + clipItem.width) < (0 - clipsModel.cacheBufferPx)) {
                        return null
                    }

                    if (clipItem.x > (clipsContainer.width + clipsModel.cacheBufferPx)) {
                        return null
                    }

                    //! NOTE This optimization is disabled, it is probably not needed,
                    // and if it needs to be enabled, it should be modified, add handlers

                    // if (clipItem.width < 24) {
                    //     return clipSmallComp
                    // }

                    return clipComp
                }
            }
        }

        Component {
            id: clipSmallComp

            ClipItemSmall {

                clipColor: clipItem.color
                collapsed: trackViewState.isTrackCollapsed
            }
        }

        Component {
            id: clipComp

            ClipItem {
                id: item

                context: root.context
                canvas: root.canvas
                title: clipItem.title
                clipColor: clipItem.color
                currentClipStyle: clipsModel.clipStyle
                groupId: clipItem.groupId
                clipKey: clipItem.key
                clipTime: clipItem.time
                pitch: clipItem.pitch
                speedPercentage: clipItem.speedPercentage
                clipSelected: clipItem.selected
                isMultiSelectionActive: root.isMultiSelectionActive
                isDataSelected: root.isDataSelected
                moveActive: root.moveActive
                isAudible: root.isTrackAudible
                multiSampleEdit: clipsContainer.multiSampleEdit
                altPressed: root.altPressed
                selectionInProgress: root.selectionInProgress || root.selectionEditInProgress
                asymmetricStereoHeightsPossible: clipsModel.asymmetricStereoHeightsPossible

                //! NOTE: use the same integer rounding as in WaveBitmapCache
                selectionStart: root.context.selectionStartPosition < clipItem.x ? 0 : Math.floor(root.context.selectionStartPosition - clipItem.x + 0.5)
                selectionWidth: root.context.selectionStartPosition < clipItem.x ?
                                    Math.round(root.context.selectionEndPosition - clipItem.x) : Math.floor(root.context.selectionEndPosition - clipItem.x + 0.5)  - Math.floor(root.context.selectionStartPosition - clipItem.x + 0.5)

                leftVisibleMargin: clipItem.leftVisibleMargin
                rightVisibleMargin: clipItem.rightVisibleMargin
                collapsed: trackViewState.isTrackCollapsed
                channelHeightRatio: trackViewState.channelHeightRatio
                showChannelSplitter: isStereo

                navigation.name: Boolean(clipItem) ? clipItem.title + clipItem.index : ""
                navigation.panel: root.navigationPanel
                navigation.column: index
                navigation.row: root.trackIdx
                navigation.accessible.name: Boolean(clipItem) ? clipItem.title : ""
                navigation.onActiveChanged: {
                    if (navigation.active) {
                        root.context.insureVisible(root.context.positionToTime(clipItem.x))
                        root.insureVerticallyVisible(root.y, root.y + root.height)
                    }
                }

                distanceToLeftNeighbor: {
                    let leftNeighbor = clipsModel.prev(clipItem.key)
                    if (!leftNeighbor) {
                        return -1
                    }
                    return clipItem.x - (leftNeighbor.x + leftNeighbor.width)
                }
                distanceToRightNeighbor: {
                    let rightNeighbor = clipsModel.next(clipItem.key)
                    if (!rightNeighbor) {
                        return -1
                    }
                    return rightNeighbor.x - (clipItem.x + clipItem.width)
                }

                onIsNearSampleChanged: function() {
                    clipsContainer.isNearSample = clipsContainer.checkIfAnyClip(function(clipItem) {
                        if (clipItem.isNearSample) {
                            clipsContainer.currentChannel = clipItem.currentChannel
                        }
                        return clipItem && clipItem.isNearSample
                    })
                }

                onHoverChanged: function() {
                    root.hover = clipsContainer.checkIfAnyClip(function(clipItem) {
                        return clipItem && clipItem.hover
                    })
                }

                onIsBrushChanged: function() {
                    clipsContainer.isBrush = clipsContainer.checkIfAnyClip(function(clipItem) {
                        return clipItem && clipItem.isBrush
                    })
                }

                onLeftTrimContainsMouseChanged: function() {
                    clipsContainer.leftTrimContainsMouse = clipsContainer.checkIfAnyClip(function(clipItem) {
                        return clipItem && clipItem.leftTrimContainsMouse
                    })
                }

                onRightTrimContainsMouseChanged: function() {
                    clipsContainer.rightTrimContainsMouse = clipsContainer.checkIfAnyClip(function(clipItem) {
                        return clipItem && clipItem.rightTrimContainsMouse
                    })
                }

                onLeftTrimPressedButtonsChanged: function() {
                    clipsContainer.leftTrimPressedButtons = clipsContainer.checkIfAnyClip(function(clipItem) {
                        return clipItem && clipItem.leftTrimPressedButtons
                    })
                }

                onRightTrimPressedButtonsChanged: function() {
                    clipsContainer.rightTrimPressedButtons = clipsContainer.checkIfAnyClip(function(clipItem) {
                        return clipItem && clipItem.rightTrimPressedButtons
                    })
                }

                onClipHeaderHoveredChanged: function(headerHovered) {
                    root.clipHeaderHoveredChanged(headerHovered)
                }

                onClipStartEditRequested: function() {
                    clipsModel.startEditClip(clipItem.key)
                }

                onClipEndEditRequested: function() {
                    clipsModel.endEditClip(clipItem.key)

                    root.triggerClipGuideline(false, -1)
                }

                onClipLeftTrimRequested: function(completed, action) {
                    clipsModel.trimLeftClip(clipItem.key, completed, action)

                    handleClipGuideline(clipItem.key, Direction.Left, completed)
                }

                onClipRightTrimRequested: function(completed, action) {
                    clipsModel.trimRightClip(clipItem.key, completed, action)

                    handleClipGuideline(clipItem.key, Direction.Right, completed)
                }

                onClipLeftStretchRequested: function(completed, action) {
                    clipsModel.stretchLeftClip(clipItem.key, completed, action)

                    handleClipGuideline(clipItem.key, Direction.Left, completed)
                }

                onClipRightStretchRequested: function(completed, action) {
                    clipsModel.stretchRightClip(clipItem.key, completed, action)

                    handleClipGuideline(clipItem.key, Direction.Right, completed)
                }

                onStartAutoScroll: {
                    root.context.startAutoScroll(root.context.mousePositionTime())
                }

                onStopAutoScroll: {
                    root.context.stopAutoScroll()
                }

                onClipItemMousePositionChanged: function(xWithinClip, yWithinClip) {
                    var yWithinTrack = yWithinClip
                    var xWithinTrack = xWithinClip + clipItem.x

                    trackItemMousePositionChanged(xWithinTrack, yWithinTrack, clipItem.key)

                    let time = root.context.findGuideline(root.context.positionToTime(xWithinTrack, true))
                    root.triggerClipGuideline(time, false)
                }

                onRequestSelected: {
                    clipsModel.selectClip(clipItem.key)
                    root.clipSelectedRequested()
                }

                onRequestSelectionReset: {
                    clipsModel.resetSelectedClips()
                    root.selectionResetRequested()
                }

                onTitleEditStarted: {
                    clipsModel.selectClip(clipItem.key)
                }

                onTitleEditAccepted: function(newTitle) {
                    root.changeClipTitle(clipItem.key, newTitle)
                }

                onTitleEditCanceled: {
                    clipsModel.resetSelectedClip()
                }

                onSplitterPositionChangeRequested: function (position) {
                    clipsContainer.updateChannelHeightRatio(position)
                    clipsContainer.resetTargetHeightRatio()
                }

                onPitchChangeRequested: {
                    Qt.callLater(clipsModel.openClipPitchEdit, clipItem.key)
                }

                onPitchResetRequested: {
                    Qt.callLater(clipsModel.resetClipPitch, clipItem.key)
                }

                onSpeedChangeRequested: {
                    Qt.callLater(clipsModel.openClipSpeedEdit, clipItem.key)
                }

                onSpeedResetRequested: {
                    Qt.callLater(clipsModel.resetClipSpeed, clipItem.key)
                }

                Connections {
                    target: clipItem
                    function onWaveChanged() {
                        updateWave()
                    }

                    function onTitleEditRequested() {
                        item.editTitle()
                    }
                }
            }
        }
    }

    // this one is transparent, it's on top of the clips
    // to have extend/reduce selection area handles
    ClipsSelection {
        id: clipsSelection

        isDataSelected: root.isDataSelected
        selectionInProgress: root.selectionInProgress
        context: root.context

        anchors.fill: parent
        z: 1

        onSelectionDraged: function(x1, x2, completed) {
            root.selectionDraged(x1, x2, completed)
            if (completed) {
                root.seekToX(Math.min(x1, x2))
            }
        }

        onRequestSelectionContextMenu: function(x, y) {
            let position = mapToItem(root.parent, Qt.point(x, y))
            root.requestSelectionContextMenu(position.x, position.y)
        }

        onHandleGuideline: function(x, completed) {
            root.handleTimeGuideline(x, completed)
        }
    }

    // this one is drawn below the clips
    Rectangle {
        id: clipSelectionRectangle

        x: root.context.selectionStartPosition
        width: root.context.selectionEndPosition - x

        anchors.top: root.top
        anchors.bottom: root.bottom
        anchors.bottomMargin: sep.thickness

        visible: root.isDataSelected
        color: "#ABE7FF"
        opacity: 0.3
    }

    Rectangle {
        id: selectedTrackHighlight

        anchors.fill: parent
        anchors.bottomMargin: sep.thickness
        anchors.leftMargin: -canvas.anchors.leftMargin

        color: "#FFFFFF"
        opacity: 0.10

        visible: root.isDataSelected || root.isTrackSelected
    }

    Rectangle {
        id: defaultTrackHighlight

        anchors.fill: parent
        anchors.bottomMargin: sep.thickness
        anchors.leftMargin: -canvas.anchors.leftMargin

        color: "#FFFFFF"
        opacity: 0.05
    }

    MouseArea {
        id: dragArea

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        height: 4

        cursorShape: Qt.SizeVerCursor

        visible: !root.selectionInProgress

        onPressed: {
            root.interactionStarted()
        }

        onPositionChanged: function(mouse) {
            const resizeVerticalMargin = 10
            mouse.accepted = true

            const currentY = mapToItem(container, 0, 0).y - container.y

            const maxPosition = container.height - resizeVerticalMargin - height
            const minPosition = resizeVerticalMargin
            const newPosition = Math.max(Math.min(currentY + mouse.y, maxPosition), minPosition)

            const delta = newPosition - currentY
            trackViewState.changeTrackHeight(delta)
        }

        onReleased: {
            root.interactionEnded()
        }
    }

    ChannelSplitter {
        id: channelSplitter

        anchors.fill: parent
        anchors.topMargin: trackViewState.isTrackCollapsed ? 1 : 21
        anchors.bottomMargin: 3

        channelHeightRatio: trackViewState.channelHeightRatio
        color: "#FFFFFF"
        opacity: 0.05
        visible: isStereo

        onPositionChangeRequested: function(position) {
            clipsContainer.updateChannelHeightRatio(position)
            clipsContainer.resetTargetHeightRatio()
        }
    }

    Rectangle {
        id: trackFocusState

        anchors.fill: parent
        anchors.leftMargin: -border.width - canvas.anchors.leftMargin
        anchors.rightMargin: -border.width
        anchors.topMargin: -border.width

        visible: isTrackFocused

        color: "transparent"

        border.color: "#7EB1FF"
        border.width: 2
    }

    SeparatorLine {
        id: sep

        color: "transparent"
        anchors.bottom: parent.bottom
        thickness: 2
    }

    Connections {
        target: root.container

        function onClipMoveRequested(clipKey, completed) {
            // this one notifies every ClipListModel about moveActive
            root.updateMoveActive(completed)

            // this one moves the clips
            let clipMovedToOtherTrack = clipsModel.moveSelectedClips(clipKey, completed)

            // clip might change its' track, we need to update grabbed clipKey
            if (clipMovedToOtherTrack) {
                clipKey = clipsModel.updateClipTrack(clipKey)
                setHoveredClipKey(clipsModel.updateClipTrack(clipKey));
            }

            handleClipGuideline(clipKey, Direction.Auto, completed)
        }

        function onClipStartEditRequested(clipKey) {
            clipsModel.startEditClip(clipKey)
        }

        function onClipEndEditRequested(clipKey) {
            clipsModel.endEditClip(clipKey)
        }

        function onStartAutoScroll() {
            root.context.startAutoScroll(root.context.mousePositionTime())
        }

        function onStopAutoScroll() {
            root.context.stopAutoScroll()
        }
    }

    function handleClipGuideline(clipKey, direction, completed) {
        let guidelinePos = clipsModel.findGuideline(clipKey, direction)
        if (guidelinePos) {
            triggerClipGuideline(guidelinePos, completed)
        }
    }
}
