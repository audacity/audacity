import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Item {

    id: root

    property alias trackId: clipsModel.trackId
    property alias context: clipsModel.context

    property bool isDataSelected: false

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

                    if (clipItem.width < 24) {
                        return clipSmallComp
                    }

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
                title: clipItem.title
                clipColor: clipItem.color
                clipKey: clipItem.key
                clipTime: clipItem.time
                clipSelected: clipItem.selected
                leftVisibleMargin: clipItem.leftVisibleMargin
                rightVisibleMargin: clipItem.rightVisibleMargin
                collapsed: trackViewState.isTrackCollapsed

                onClipMoved: function(deltaX, completed) {
                    clipsModel.moveClip(clipItem.key, deltaX, completed)
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

    SeparatorLine {
        id: sep
        color: "#FFFFFF"
        opacity: 0.1
        anchors.bottom: parent.bottom
    }
}
