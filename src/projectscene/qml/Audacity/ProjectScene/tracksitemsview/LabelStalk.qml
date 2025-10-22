import QtQuick

Rectangle {
    id: root

    property bool isForPoint: false
    property bool isRight: false
    property bool enableCursorInteraction: true
    property color backgroundColor: "transparent"
    property bool isSelected: false

    signal headerHoveredChanged(bool value)
    signal labelItemMousePositionChanged(real x, real y)
    signal requestSelected()

    signal stretchRequested(bool completed)
    signal stretchStartRequested()
    signal stretchEndRequested()
    signal stretchMousePositionChanged(real x, real y)

    width: 1
    height: parent.height
    x: root.isForPoint ? -width/2 : (root.isRight ? parent.width : -1)

    color: root.backgroundColor

    MouseArea {
        id: stalkArea
        anchors.fill: parent
        anchors.leftMargin: -1
        anchors.rightMargin: -1

        acceptedButtons: Qt.LeftButton
        hoverEnabled: true
        cursorShape: !root.isForPoint ? Qt.SizeHorCursor : Qt.SizeHorCursor

        visible: root.enableCursorInteraction

        onContainsMouseChanged: {
            if (!root.visible) {
                return
            }
            root.headerHoveredChanged(containsMouse)
        }

        onPressed: function (e) {
            if (!root.isForPoint) {
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
            if (pressed && !root.isForPoint) {
                let mousePos = mapToItem(root.parent, e.x, e.y)
                root.stretchMousePositionChanged(mousePos.x, mousePos.y)
                root.stretchRequested(false)
                e.accepted = true
            } else {
                root.labelItemMousePositionChanged(e.x, e.y)
                e.accepted = false
            }
        }

        onReleased: function (e) {
            if (root.isSelected && !root.isForPoint) {
                root.stretchRequested(true)
                root.stretchEndRequested()
                e.accepted = true
            } else {
                e.accepted = false
            }
        }
    }
}
