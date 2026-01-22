import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Item {
    id: root

    property NavigationSection navigationSection: null
    property NavigationPanel navigationPanel: null

    property var trackId: null
    property var context: null
    property var canvas: null
    property var container: null

    property bool isDataSelected: false
    property bool isTrackSelected: false
    property bool isTrackFocused: false
    property bool isMultiSelectionActive: false
    property bool isTrackAudible: true
    property bool moveActive: false
    property bool altPressed: false
    property bool ctrlPressed: false
    property bool selectionEditInProgress: false
    property bool selectionInProgress: false
    property bool hover: false

    property alias bottomSeparatorHeight: sep.height

    property alias contentItem: contentLoader.item

    signal interactionStarted
    signal interactionEnded
    signal trackItemMousePositionChanged(real x, real y, var itemKey)
    signal setHoveredItemKey(var itemKey)

    signal itemHeaderHoveredChanged(bool val)

    signal itemSelectedRequested
    signal selectionResetRequested
    signal requestSelectionContextMenu(real x, real y)
    signal selectionResize(var x1, var x2, var completed)

    signal updateMoveActive(bool completed)

    signal seekToX(var x)
    signal insureVerticallyVisible(var top, var bottom)

    signal handleTimeGuideline(real x, bool completed)
    signal triggerItemGuideline(real x, bool completed)
    signal itemDragEditCanceled

    signal initRequired

    property Component contentComponent: null

    height: trackViewState.trackHeight

    property TrackViewStateModel trackViewState: TrackViewStateModel {
        trackId: root.trackId
    }

    function init() {
        trackViewState.init()

        root.initRequired()
    }

    Loader {
        id: contentLoader
        anchors.fill: parent
        z: 1
        sourceComponent: root.contentComponent
    }

    // Selection highlight
    Rectangle {
        id: selectionRectangle

        x: root.context ? root.context.selectionStartPosition : 0
        width: root.context ? (root.context.selectionEndPosition - x) : 0

        anchors.top: root.top
        anchors.bottom: root.bottom
        anchors.bottomMargin: sep.thickness

        visible: root.isDataSelected
        color: ui.theme.extra["selection_highlight_color"]
        opacity: 0.3
    }

    Rectangle {
        id: selectedTrackHighlight

        anchors.fill: parent
        anchors.bottomMargin: sep.thickness
        anchors.leftMargin: (canvas && canvas.anchors && canvas.anchors.leftMargin) ? -canvas.anchors.leftMargin : 0

        color: ui.theme.extra["white_color"]
        opacity: 0.10

        visible: root.isDataSelected || root.isTrackSelected
    }

    Rectangle {
        id: defaultTrackHighlight

        anchors.fill: parent
        anchors.bottomMargin: sep.thickness
        anchors.leftMargin: (canvas && canvas.anchors && canvas.anchors.leftMargin) ? -canvas.anchors.leftMargin : 0

        color: ui.theme.extra["white_color"]
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

        onPositionChanged: function (mouse) {
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

    Rectangle {
        id: trackFocusState

        anchors.fill: parent
        anchors.leftMargin: -border.width - ((canvas && canvas.anchors && canvas.anchors.leftMargin) ? canvas.anchors.leftMargin : 0)
        anchors.rightMargin: -border.width
        anchors.topMargin: -border.width

        visible: isTrackFocused

        color: "transparent"

        border.color: ui.theme.extra["focus_state_color"]
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

        function onStartAutoScroll() {
            if (root.context) {
                root.context.startAutoScroll(root.context.mousePositionTime())
            }
        }

        function onStopAutoScroll() {
            if (root.context) {
                root.context.stopAutoScroll()
            }
        }
    }
}
