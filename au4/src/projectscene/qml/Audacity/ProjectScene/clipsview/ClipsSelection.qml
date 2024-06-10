import QtQuick

Item {
    id: root

    property alias active: selRect.visible
    property real minSelection: 12 // px  4left + 4 + 4right

    function onSelectionStarted() {
        selRect.visible = false
        leftMa.enabled = false
        rightMa.enabled = false
    }

    function onSelectionChanged(p1, p2) {
        if (p2.x > p1.x) {
            selRect.x = p1.x
            selRect.width = p2.x - p1.x
        } else {
            selRect.x = p2.x
            selRect.width = p1.x - p2.x
        }

        selRect.visible = selRect.width > root.minSelection
    }

    function onSelectionEnded(p1, p2) {
        if (selRect.visible) {
            leftMa.x = selRect.x
            leftMa.enabled = true

            rightMa.x = selRect.x + selRect.width - rightMa.width
            rightMa.enabled = true
        }
    }

    Rectangle {
        id: selRect

        anchors.top: parent.top
        anchors.bottom: parent.bottom

        visible: false
        color: "#8EC9FF"

        opacity: 0.1
    }

    MouseArea {
        id: leftMa

        anchors.top: parent.top
        anchors.bottom: parent.bottom

        width: 4
        cursorShape: Qt.SizeHorCursor

        property real startX: 0
        property real startW: 0

        onPressed: function(mouse) {
            leftMa.cursorShape = Qt.ArrowCursor
            leftMa.startX = selRect.x
            leftMa.startW = selRect.width
        }

        onPositionChanged: function(mouse) {
            var newWidth = leftMa.startW + (mouse.x * -1)
            if (newWidth > root.minSelection) {
                selRect.x = leftMa.startX + mouse.x
                selRect.width = newWidth
            }
        }

        onReleased: {
            root.selected(selRect.x, selRect.x + selRect.width)
            leftMa.x = selRect.x
            leftMa.cursorShape = Qt.SizeHorCursor
        }
    }

    MouseArea {
        id: rightMa

        anchors.top: parent.top
        anchors.bottom: parent.bottom

        width: 4
        cursorShape: Qt.SizeHorCursor

        property real startX: 0
        property real startW: 0

        onPressed: function(mouse) {
            rightMa.cursorShape = Qt.ArrowCursor
            rightMa.startW = selRect.width
        }

        onPositionChanged: function(mouse) {
            var newWidth = rightMa.startW + mouse.x
            if (newWidth > root.minSelection) {
                selRect.width = newWidth
            }
        }

        onReleased: {
            root.selected(selRect.x, selRect.x + selRect.width)
            rightMa.x = selRect.x + selRect.width - rightMa.width
            rightMa.cursorShape = Qt.SizeHorCursor
        }
    }
}
