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
                property int cacheBuffer: 200 // px

                sourceComponent: {
                    if ((clipItem.x + clipItem.width) < (0 - cacheBuffer)) {
                        return null
                    }

                    if (clipItem.x > (clipsContaner.width + cacheBuffer)) {
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
                clipStartTime: root.context.positionToTime(clipItem.x)
                clipColor: clipItem.color
                clipKey: clipItem.key
                clipSelected: clipItem.selected
                collapsed: trackViewState.isTrackCollapsed

                dragMaximumX: clipItem.moveMaximumX + borderWidth
                dragMinimumX: clipItem.moveMinimumX - borderWidth

                onPositionChanged: function(x) {
                    clipsModel.modeClip(clipItem.key, x)
                }

                onRequestSelected: {
                    clipsModel.selectClip(clipItem.key)
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

                onClipItemMousePositionChanged: function(xWithinClip, yWithinClip) {
                    var yWithinTrack = yWithinClip
                    var xWithinTrack = xWithinClip + clipItem.x
                    trackItemMousePositionChanged(xWithinTrack, yWithinTrack)
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
        color: ui.theme.white
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
        anchors.bottom: parent.bottom }
}
