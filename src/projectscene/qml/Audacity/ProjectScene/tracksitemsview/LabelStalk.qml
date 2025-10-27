import QtQuick

Rectangle {
    id: root

    property bool isForPoint: false
    property bool isRight: false
    property bool enableCursorInteraction: true
    property color backgroundColor: "transparent"

    signal headerHoveredChanged(bool value)
    signal labelItemMousePositionChanged(real x, real y)
    signal requestSelected()

    width: 1
    height: parent.height
    x: root.isForPoint ? -width/2 : (root.isRight ? parent.width : -1)

    color: root.backgroundColor

    MouseArea {
        anchors.fill: parent
        anchors.leftMargin: -1
        anchors.rightMargin: -1

        acceptedButtons: Qt.LeftButton
        hoverEnabled: true
        cursorShape: pressed ? Qt.CloseHandCursor : Qt.OpenHandCursor

        visible: root.enableCursorInteraction

        onContainsMouseChanged: {
            if (!root.visible) {
                return
            }
            root.headerHoveredChanged(containsMouse)
        }

        onPressed: function (e) {
            root.requestSelected()
            e.accepted = false
        }

        onPositionChanged: function (e) {
            root.labelItemMousePositionChanged(e.x, e.y)
            e.accepted = false
        }

        onReleased: function (e) {
            e.accepted = false
        }
    }
}
