import QtQuick

Rectangle {
    id: root

    property bool isForPoint: false
    property bool isRight: false
    property bool enableCursorInteraction: true
    property color backgroundColor: "transparent"

    signal headerHoveredChanged(bool value)

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
    }
}
