import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Item {

    id: root

    property alias trackId: clipsModel.trackId
    property alias context: clipsModel.context
    property var canvas: null

    property bool isDataSelected: false
    property bool isStereo: clipsModel.isStereo
    property double channelHeightRatio: isStereo ? 0.5 : 1

    signal interactionStarted()
    signal interactionEnded()
    // mouse position event is not propagated on overlapping mouse areas
    // so we are handling it manually
    signal trackItemMousePositionChanged(real x, real y)
    signal clipSelectedRequested()

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
                width: clipItem.width
                x: clipItem.x

                asynchronous: true

                sourceComponent: {
                    if ((clipItem.x + clipItem.width) < (0 - clipsModel.cacheBufferPx)) {
                        return null
                    }

                    if (clipItem.x > (clipsContaner.width + clipsModel.cacheBufferPx)) {
                        return null
                    }

                    if (clipItem.width < 2) {
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
                leftVisibleMargin: clipItem.leftVisibleMargin
                rightVisibleMargin: clipItem.rightVisibleMargin
                collapsed: trackViewState.isTrackCollapsed
                channelHeightRatio: root.channelHeightRatio
                showChannelSplitter: isStereo

                onClipMoved: function(deltaX, completed) {
                    clipsModel.moveClip(clipItem.key, deltaX, completed)
                }

                onClipLeftTrimmed: function(deltaX, posOnCanvas) {
                    clipsModel.trimLeftClip(clipItem.key, deltaX, posOnCanvas)
                }

                onClipRightTrimmed: function(deltaX, posOnCanvas) {
                    clipsModel.trimRightClip(clipItem.key, deltaX, posOnCanvas)
                }

                onClipItemMousePositionChanged: function(xWithinClip, yWithinClip) {
                    var yWithinTrack = yWithinClip
                    var xWithinTrack = xWithinClip + clipItem.x
                    trackItemMousePositionChanged(xWithinTrack, yWithinTrack)
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

    Rectangle {
        id: selRect
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        color: "#8EC9FF"
        opacity: 0.4
        visible: root.isDataSelected

        x: root.context.timeToPosition(root.context.selectionStartTime)
        width: root.context.timeToPosition(root.context.selectionEndTime) - x
        z: 1
    }

    Rectangle {
        id: selectedHighlight
        z: 0
        anchors.fill: parent
        color: "#FFFFFF"
        opacity: 0.05
        visible: root.isDataSelected
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
        separatorWidth: 2
    }
}
