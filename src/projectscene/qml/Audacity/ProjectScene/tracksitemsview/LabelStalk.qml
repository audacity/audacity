import QtQuick

Rectangle {
    id: root

    property bool isForPoint: false
    property bool isRight: false
    property bool enableCursorInteraction: true
    property color backgroundColor: "transparent"
    property bool isSelected: false

    property alias hovered: dragArea.containsMouse

    signal headerHoveredChanged(bool value)
    signal requestSelected

    signal mousePositionChanged(real x, real y)

    width: 1

    color: root.backgroundColor

    MouseArea {
        id: dragArea
        anchors.fill: parent
        anchors.leftMargin: -1
        anchors.rightMargin: -1

        acceptedButtons: Qt.LeftButton
        hoverEnabled: true
        cursorShape: Qt.OpenHandCursor

        visible: root.enableCursorInteraction

        onContainsMouseChanged: {
            if (!root.visible) {
                return
            }
            root.headerHoveredChanged(containsMouse)
        }

        //! NOTE Like the label header, the stalk is a move handle: the press is only used
        // to select the label and is then passed through to the items view, which handles
        // the actual move (and switches the cursor to the closed hand).
        onPressed: function (e) {
            root.requestSelected()
            e.accepted = false
        }

        onPositionChanged: function (e) {
            let mousePos = mapToItem(root.parent, e.x, e.y)
            root.mousePositionChanged(mousePos.x, mousePos.y)
            e.accepted = false
        }

        onReleased: function (e) {
            e.accepted = false
        }
    }
}
