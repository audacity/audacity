import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Item {

    id: root

    property alias trackId: clipsModel.trackId
    property alias context: clipsModel.context
    property var canvas: null
    property var container: null

    property bool isDataSelected: false
    property bool isTrackSelected: false
    property bool isMultiSelectionActive: false
    property bool isTrackAudible: true
    property bool isStereo: clipsModel.isStereo
    property double channelHeightRatio: isStereo ? 0.5 : 1
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

    signal clipHeaderHoveredChanged(bool val)

    height: trackViewState.trackHeight

    ClipsListModel {
        id: clipsModel
    }

    TracksViewStateModel {
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
                if (clipsContainer.isNearSample) {

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
                channelHeightRatio: root.channelHeightRatio
                showChannelSplitter: isStereo
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
                }

                onClipLeftTrimRequested: function(completed) {
                    clipsModel.trimLeftClip(clipItem.key, completed)
                }

                onClipRightTrimRequested: function(completed) {
                    clipsModel.trimRightClip(clipItem.key, completed)
                }

                onClipLeftStretchRequested: function(completed) {
                    clipsModel.stretchLeftClip(clipItem.key, completed)
                }

                onClipRightStretchRequested: function(completed) {
                    clipsModel.stretchRightClip(clipItem.key, completed)
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

                onRatioChanged: function (ratio) {
                    root.channelHeightRatio = ratio
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
    }

    // this one is drawn below the clips
    Rectangle {
        id: clipSelectionRectangle

        x: root.context.selectionStartPosition
        width: root.context.selectionEndPosition - x

        anchors.top: root.top
        anchors.bottom: root.bottom

        visible: root.isDataSelected
        color: "#ABE7FF"
        opacity: 0.3
    }

    Rectangle {
        id: selectedTrackHighlight
        z: 0
        anchors.fill: parent
        color: "#FFFFFF"
        opacity: 0.05
        visible: root.isDataSelected || root.isTrackSelected
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

        channelHeightRatio: root.channelHeightRatio
        color: "#FFFFFF"
        opacity: 0.05
        visible: isStereo

        onRatioChanged: function(ratio) {
            root.channelHeightRatio = ratio
        }
    }

    SeparatorLine {
        id: sep
        color: "#FFFFFF"
        opacity: 0.1
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
                setHoveredClipKey(clipsModel.updateClipTrack(clipKey));
            }
        }

        function onClipStartEditRequested(clipKey) {
            clipsModel.startEditClip(clipKey)
        }

        function onStartAutoScroll() {
            root.context.startAutoScroll(root.context.mousePositionTime())
        }

        function onStopAutoScroll() {
            root.context.stopAutoScroll()
        }
    }
}
