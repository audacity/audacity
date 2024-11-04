import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Item {

    id: root

    property alias trackId: clipsModel.trackId
    property alias context: clipsModel.context
    property var canvas: null

    property bool isDataSelected: false
    property bool isTrackSelected: false
    property bool isStereo: clipsModel.isStereo
    property double channelHeightRatio: isStereo ? 0.5 : 1

    signal interactionStarted()
    signal interactionEnded()
    // mouse position event is not propagated on overlapping mouse areas
    // so we are handling it manually
    signal trackItemMousePositionChanged(real x, real y, var clipKey)
    signal clipSelectedRequested()

    signal selectionDraged(var x1, var x2, var completed)
    signal seekToX(var x)

    height: trackViewState.trackHeight

    ClipsListModel {
        id: clipsModel

        onRequestClipTitleEdit: function(index){
            repeator.itemAt(index).editTitle()
        }
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

    Item {
        id: clipsContaner
        anchors.fill: parent
        anchors.bottomMargin: sep.height
        z: 1

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

                    if (clipItem.x > (clipsContaner.width + clipsModel.cacheBufferPx)) {
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

                context: root.context
                canvas: root.canvas
                title: clipItem.title
                clipColor: clipItem.color
                clipKey: clipItem.key
                clipTime: clipItem.time
                clipSelected: clipItem.selected
                isDataSelected: root.isDataSelected
                selectionStart: root.context.selectionStartPosition < clipItem.x ? 0 : root.context.selectionStartPosition - clipItem.x
                selectionWidth: root.context.selectionStartPosition < clipItem.x ?
                                    root.context.selectionEndPosition - clipItem.x : root.context.selectionEndPosition - root.context.selectionStartPosition
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

                onClipStartEditRequested: function() {
                    clipsModel.startEditClip(clipItem.key)
                }

                onClipEndEditRequested: function() {
                    clipsModel.endEditClip(clipItem.key)
                }

                onClipMoveRequested: function(completed) {
                    clipsModel.moveClip(clipItem.key, completed)
                }

                onClipLeftTrimRequested: function(completed) {
                    clipsModel.trimLeftClip(clipItem.key, completed)
                }

                onClipRightTrimRequested: function(completed) {
                    clipsModel.trimRightClip(clipItem.key, completed)
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
            }
        }
    }

    // this one is transparent, it's on top of the clips
    // to have extend/reduce selection area handles
    ClipsSelection {
        id: clipsSelection

        isDataSelected: root.isDataSelected
        context: root.context

        anchors.fill: parent
        z: 1

        onSelectionDraged: function(x1, x2, completed) {
            root.selectionDraged(x1, x2, completed)
            if (completed) {
                root.seekToX(Math.min(x1, x2))
            }
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

        onPressed: {
            root.interactionStarted()
        }

        onPositionChanged: function(mouse) {
            mouse.accepted = true
            trackViewState.changeTrackHeight(mouse.y)
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
}
