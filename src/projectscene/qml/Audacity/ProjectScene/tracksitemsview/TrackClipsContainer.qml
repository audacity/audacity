import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

TrackItemsContainer {
    id: root

    property bool isBrush: false
    property bool isIsolationMode: false
    property bool isNearSample: false
    property bool leftTrimContainsMouse: false
    property bool rightTrimContainsMouse: false
    property bool leftTrimPressedButtons: false
    property bool rightTrimPressedButtons: false

    TrackClipsListModel {
        id: clipsModel
        trackId: root.trackId
        context: root.context
    }

    onInitRequired: function() {
        clipsModel.init()
    }

    function changeClipTitle(index, newTitle) {
        clipsModel.changeClipTitle(index, newTitle)
        clipsModel.resetSelectedObjects()
    }

    onCtrlPressedChanged: {
        if (!root.ctrlPressed && contentItem) {
            contentItem.clipsContainer.mapToAllClips({x: 0, y: 0}, function(clipItem, mouseEvent) {
                clipItem.isIsolationMode = false
            })
        }
    }

    contentComponent: Component {
        Item {
            property alias clipsContainer: clipsContainer

            //! clip
            Item {
                id: clipsContainer

                anchors.fill: parent
                anchors.bottomMargin: root.bottomSeparatorHeight
                z: 1

                property double targetHeightRatio: 0.5
                readonly property int minChannelHeight: 20
                readonly property int yMinValue: Math.min(root.height / 2, minChannelHeight)
                readonly property int yMaxValue: Math.max(root.height / 2, root.height - minChannelHeight);

                property bool multiSampleEdit: false
                property int currentChannel: 0

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
                    root.trackViewState.changeChannelHeightRatio(newY / height)
                }

                function resetTargetHeightRatio() {
                    targetHeightRatio = root.trackViewState.channelHeightRatio
                }

                onHeightChanged: {
                    updateChannelHeightRatio(clipsContainer.targetHeightRatio * clipsContainer.height)
                }

                MouseArea {
                    id: clipsContainerMouseArea
                    propagateComposedEvents: true
                    hoverEnabled: true
                    pressAndHoldInterval: 0
                    enabled: !root.selectionInProgress && (root.isNearSample || root.isBrush)
                    cursorShape: root.selectionEditInProgress ? Qt.SizeHorCursor : Qt.IBeamCursor 

                    anchors.fill: parent

                    onDoubleClicked: function(e) {
                        e.accepted = true
                    }

                    onPressAndHold: function(e) {
                        if (root.isNearSample || root.altPressed) {

                            if (root.ctrlPressed) {
                                root.isIsolationMode = true
                            }
                            else if (!root.altPressed) {
                                root.multiSampleEdit = true
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
                        root.isIsolationMode = false

                        clipsContainer.mapToAllClips(e, function(clipItem, mouseEvent) {
                            clipItem.multiSampleEdit = false
                            root.isIsolationMode = false
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
                            clipItem.setContainsMouse(containsMouse)
                        })
                    }
                }

                Repeater {
                    id: repeator

                    model: clipsModel

                    delegate: Loader {
                        id: loader

                        property var itemData: model.item
                        property int index: model.index

                        height: parent.height
                        width: Math.max(3, itemData.width)
                        x: itemData.x

                        asynchronous: true

                        sourceComponent: {
                            if ((itemData.x + itemData.width) < (0 - clipsModel.cacheBufferPx)) {
                                return null
                            }

                            if (itemData.x > (clipsContainer.width + clipsModel.cacheBufferPx)) {
                                return null
                            }

                            //! NOTE This optimization is disabled, it is probably not needed,
                            // and if it needs to be enabled, it should be modified, add handlers

                            // if (itemData.width < 24) {
                            //     return clipSmallComp
                            // }

                            return clipComp
                        }

                        Component {
                            id: clipSmallComp

                            ClipItemSmall {
                                property var itemData: loader.itemData
                                property int index: loader.index

                                clipColor: itemData.color
                                collapsed: root.trackViewState.isTrackCollapsed
                            }
                        }

                        Component {
                            id: clipComp

                            ClipItem {
                                id: item

                                property var itemData: loader.itemData
                                property int index: loader.index

                                context: root.context
                                canvas: root.canvas

                                title: itemData.title
                                clipColor: itemData.color
                                groupId: itemData.groupId
                                clipKey: itemData.key
                                clipTime: itemData.time
                                pitch: itemData.pitch

                                currentClipStyle: clipsModel.clipStyle

                                speedPercentage: itemData.speedPercentage
                                clipSelected: itemData.selected
                                isMultiSelectionActive: root.isMultiSelectionActive
                                isDataSelected: root.isDataSelected
                                moveActive: root.moveActive
                                isAudible: root.isTrackAudible
                                multiSampleEdit: clipsContainer.multiSampleEdit
                                altPressed: root.altPressed
                                selectionInProgress: root.selectionInProgress || root.selectionEditInProgress
                                asymmetricStereoHeightsPossible: clipsModel.asymmetricStereoHeightsPossible

                                //! NOTE: use the same integer rounding as in WaveBitmapCache
                                selectionStart: root.context.selectionStartPosition < itemData.x ? 0 : Math.floor(root.context.selectionStartPosition - itemData.x + 0.5)
                                selectionWidth: root.context.selectionStartPosition < itemData.x ?
                                                    Math.round(root.context.selectionEndPosition - itemData.x) : Math.floor(root.context.selectionEndPosition - itemData.x + 0.5)  - Math.floor(root.context.selectionStartPosition - itemData.x + 0.5)

                                leftVisibleMargin: itemData.leftVisibleMargin
                                rightVisibleMargin: itemData.rightVisibleMargin
                                collapsed: root.trackViewState.isTrackCollapsed
                                channelHeightRatio: root.trackViewState.channelHeightRatio
                                showChannelSplitter: clipsModel.isStereo

                                navigation.name: Boolean(itemData) ? itemData.title + itemData.index : ""
                                navigation.panel: root.navigationPanel
                                navigation.column: itemData ? Math.floor(itemData.x) : 0
                                navigation.accessible.name: Boolean(itemData) ? itemData.title : ""
                                navigation.onActiveChanged: {
                                    if (navigation.active) {
                                        root.context.insureVisible(root.context.positionToTime(itemData.x))
                                        root.insureVerticallyVisible(root.y, root.y + root.height)
                                    }
                                }

                                distanceToLeftNeighbor: {
                                    let leftNeighbor = clipsModel.prev(itemData.key)
                                    if (!leftNeighbor) {
                                        return -1
                                    }
                                    return itemData.x - (leftNeighbor.x + leftNeighbor.width)
                                }
                                distanceToRightNeighbor: {
                                    let rightNeighbor = clipsModel.next(itemData.key)
                                    if (!rightNeighbor) {
                                        return -1
                                    }
                                    return rightNeighbor.x - (itemData.x + itemData.width)
                                }

                                onIsNearSampleChanged: function() {
                                    root.isNearSample = clipsContainer.checkIfAnyClip(function(clipItem) {
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
                                    root.isBrush = clipsContainer.checkIfAnyClip(function(clipItem) {
                                        return clipItem && clipItem.isBrush
                                    })
                                }

                                onLeftTrimContainsMouseChanged: function() {
                                    root.leftTrimContainsMouse = clipsContainer.checkIfAnyClip(function(clipItem) {
                                        return clipItem && clipItem.leftTrimContainsMouse
                                    })
                                }

                                onRightTrimContainsMouseChanged: function() {
                                    root.rightTrimContainsMouse = clipsContainer.checkIfAnyClip(function(clipItem) {
                                        return clipItem && clipItem.rightTrimContainsMouse
                                    })
                                }

                                onLeftTrimPressedButtonsChanged: function() {
                                    root.leftTrimPressedButtons = clipsContainer.checkIfAnyClip(function(clipItem) {
                                        return clipItem && clipItem.leftTrimPressedButtons
                                    })
                                }

                                onRightTrimPressedButtonsChanged: function() {
                                    root.rightTrimPressedButtons = clipsContainer.checkIfAnyClip(function(clipItem) {
                                        return clipItem && clipItem.rightTrimPressedButtons
                                    })
                                }

                                onHeaderHoveredChanged: function() {
                                    root.itemHeaderHoveredChanged(headerHovered)
                                }

                                onClipStartEditRequested: function() {
                                    clipsModel.startEditClip(itemData.key)
                                }

                                onClipEndEditRequested: function() {
                                    clipsModel.endEditClip(itemData.key)

                                    root.triggerClipGuideline(false, -1)
                                }

                                onClipLeftTrimRequested: function(completed, action) {
                                    clipsModel.trimLeftClip(itemData.key, completed, action)

                                    handleClipGuideline(itemData.key, Direction.Left, completed)
                                }

                                onClipRightTrimRequested: function(completed, action) {
                                    clipsModel.trimRightClip(itemData.key, completed, action)

                                    handleClipGuideline(itemData.key, Direction.Right, completed)
                                }

                                onClipLeftStretchRequested: function(completed, action) {
                                    clipsModel.stretchLeftClip(itemData.key, completed, action)

                                    handleClipGuideline(itemData.key, Direction.Left, completed)
                                }

                                onClipRightStretchRequested: function(completed, action) {
                                    clipsModel.stretchRightClip(itemData.key, completed, action)

                                    handleClipGuideline(itemData.key, Direction.Right, completed)
                                }

                                onStartAutoScroll: {
                                    root.context.startAutoScroll(root.context.mousePositionTime())
                                }

                                onStopAutoScroll: {
                                    root.context.stopAutoScroll()
                                }

                                onClipItemMousePositionChanged: function(xWithinClip, yWithinClip) {
                                    var yWithinTrack = yWithinClip
                                    var xWithinTrack = xWithinClip + itemData.x

                                    trackItemMousePositionChanged(xWithinTrack, yWithinTrack, itemData.key)

                                    let time = root.context.findGuideline(root.context.positionToTime(xWithinTrack, true))
                                    root.triggerClipGuideline(time, false)
                                }

                                onRequestSelected: {
                                    clipsModel.selectClip(itemData.key)
                                    root.itemSelectedRequested()
                                }

                                onRequestSelectionReset: {
                                    clipsModel.resetSelectedObjects()
                                    root.selectionResetRequested()
                                }

                                onTitleEditStarted: {
                                    clipsModel.selectClip(itemData.key)
                                }

                                onTitleEditAccepted: function(newTitle) {
                                    root.changeClipTitle(itemData.key, newTitle)
                                }

                                onTitleEditCanceled: {
                                    clipsModel.resetSelectedObjects()
                                }

                                onSplitterPositionChangeRequested: function (position) {
                                    clipsContainer.updateChannelHeightRatio(position)
                                    clipsContainer.resetTargetHeightRatio()
                                }

                                onPitchChangeRequested: {
                                    Qt.callLater(clipsModel.openClipPitchEdit, itemData.key)
                                }

                                onPitchResetRequested: {
                                    Qt.callLater(clipsModel.resetClipPitch, itemData.key)
                                }

                                onSpeedChangeRequested: {
                                    Qt.callLater(clipsModel.openClipSpeedEdit, itemData.key)
                                }

                                onSpeedResetRequested: {
                                    Qt.callLater(clipsModel.resetClipSpeed, itemData.key)
                                }

                                Connections {
                                    target: itemData
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
                }
            }

            // this one is transparent, it's on top of the clips
            // to have extend/reduce selection area handles
            ObjectsSelection {
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

            //! clip
            ChannelSplitter {
                id: channelSplitter

                anchors.fill: parent
                anchors.topMargin: root.trackViewState.isTrackCollapsed ? 1 : 21
                anchors.bottomMargin: 3

                channelHeightRatio: root.trackViewState.channelHeightRatio
                color: "#FFFFFF"
                opacity: 0.05
                visible: clipsModel.isStereo

                onPositionChangeRequested: function(position) {
                    clipsContainer.updateChannelHeightRatio(position)
                    clipsContainer.resetTargetHeightRatio()
                }
            }
        }
    }

    Connections {
        target: root.container

        function onItemMoveRequested(objectKey, completed) {
            // this one notifies every ClipListModel about moveActive
            root.updateMoveActive(completed);

            // this one moves the clips
            let clipMovedToOtherTrack = clipsModel.moveSelectedClips(objectKey, completed)

            // clip might change its' track, we need to update grabbed objectKey
            if (clipMovedToOtherTrack) {
                objectKey = clipsModel.updateClipTrack(objectKey)
                setHoveredObjectKey(clipsModel.updateClipTrack(objectKey));
            }

            handleClipGuideline(objectKey, Direction.Auto, completed)
        }

        function onItemStartEditRequested(objectKey) {
            clipsModel.startEditClip(objectKey)
        }

        function onItemEndEditRequested(objectKey) {
            clipsModel.endEditClip(objectKey)
        }

        function onCancelClipDragEditRequested(clipKey) {
            if (clipsModel.cancelClipDragEdit(clipKey)) {
                root.clipDragEditCanceled()
            }
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
