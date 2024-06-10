import QtQuick

Item {
    id: root

    property alias active: selRect.visible
    property real minSelection: 12 // px  4left + 4 + 4right

    signal selected(x1 : real, y1 : real, x2 : real, y2 : real)
    signal reset()

    property real startX: -1
    property real startY: -1

    function onPressed(mouse) {
        if (selRect.visible) {
            root.reset()
        }

        root.startX = mouse.x
        root.startY = mouse.y
        selRect.visible = false
        leftMa.enabled = false
        rightMa.enabled = false
    }

    function onPositionChanged(mouse) {
        if (root.startX < 0) {
            return
        }

        var x = mouse.x
        if (x > root.startX) {
            selRect.x = root.startX
            selRect.width = x - root.startX
        } else {
            selRect.x = x
            selRect.width = root.startX - x
        }

        selRect.visible = selRect.width > root.minSelection
    }

    function onReleased(mouse) {
        if (selRect.visible) {
            root.selected(root.startX, root.startY, mouse.x, mouse.y)

            leftMa.x = selRect.x
            leftMa.enabled = true

            rightMa.x = selRect.x + selRect.width - rightMa.width
            rightMa.enabled = true
        }

        root.startX = -1
        root.startY = -1
    }

    Rectangle {
        id: selRect

        anchors.top: parent.top
        anchors.bottom: parent.bottom

        visible: false
        color: "#8EC9FF"

        opacity: 0.6
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
