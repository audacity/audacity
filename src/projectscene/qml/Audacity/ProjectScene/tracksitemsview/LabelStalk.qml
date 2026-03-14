import QtQuick

Rectangle {
    id: root

    property bool isForPoint: false
    property bool isForMarker: false
    property bool isRight: false
    property bool enableCursorInteraction: true
    property color backgroundColor: "transparent"
    property bool isSelected: false

    property alias hovered: dragArea.containsMouse

    property bool isStretchInProgress: false

    signal headerHoveredChanged(bool value)
    signal requestSelected()

    signal stretchRequested(bool completed)
    signal stretchStartRequested()
    signal stretchEndRequested()
    signal stretchMousePositionChanged(real x, real y)
    signal contextMenuOpenRequested(real x, real y)

    width: 1

    color: root.backgroundColor

    MouseArea {
        id: dragArea
        anchors.fill: parent
        anchors.leftMargin: -1
        anchors.rightMargin: -1

        acceptedButtons: Qt.LeftButton | Qt.RightButton
        hoverEnabled: true
        cursorShape: root.isForMarker ? Qt.SizeAllCursor : Qt.SizeHorCursor

        visible: root.enableCursorInteraction

        onContainsMouseChanged: {
            if (!root.visible || (!root.isForPoint && !root.isForMarker)) {
                return
            }
            root.headerHoveredChanged(containsMouse)
        }

        onClicked: function (e) {
            if (e.button === Qt.RightButton) {
                root.contextMenuOpenRequested(e.x, e.y)
            }
        }

        onPressed: function (e) {
            if (e.button === Qt.RightButton) {
                return
            }
            if (!root.isForPoint || root.isForMarker) {
                root.isStretchInProgress = true

                let mousePos = mapToItem(root.parent, e.x, e.y)
                root.stretchMousePositionChanged(mousePos.x, mousePos.y)
                root.stretchStartRequested()
                root.stretchRequested(false)
                e.accepted = true
            } else {
                root.requestSelected()
                e.accepted = false
            }
        }

        onPositionChanged: function (e) {
            let mousePos = mapToItem(root.parent, e.x, e.y)

            if (pressed && (!root.isForPoint || root.isForMarker)) {
                root.stretchMousePositionChanged(mousePos.x, mousePos.y)
                root.stretchRequested(false)
                e.accepted = true
            } else {
                root.stretchMousePositionChanged(mousePos.x, mousePos.y)
                e.accepted = false
            }
        }

        onReleased: function (e) {
            if (!root.isForPoint || root.isForMarker) {
                root.isStretchInProgress = false
                root.stretchRequested(true)
                root.stretchEndRequested()
                e.accepted = true
            } else {
                e.accepted = false
            }
        }
    }
}
