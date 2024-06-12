import QtQuick

Item {
    id: root

    property alias active: selRect.visible
    property real minSelection: 12 // px  4left + 4 + 4right

    signal selectionDraged(var x1, var x2, var completed)

    function onSelectionStarted() {
        selRect.visible = false
        leftMa.visible = false
        rightMa.visible = false
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
            leftMa.visible = true

            rightMa.x = selRect.x + selRect.width - rightMa.width
            rightMa.visible = true
        }
    }

    function _onSelectionDraging(completed) {
        root.selectionDraged(selRect.x, selRect.x + selRect.width, completed)
    }

    Rectangle {
        id: selRect

        anchors.top: parent.top
        anchors.bottom: parent.bottom

        visible: false
        color: "#8EC9FF"
        opacity: 0.1

        Rectangle {
            anchors.top: parent.top
            anchors.bottom: parent.bottom
            anchors.left: parent.left
            width: 2
            color: "#8EC9FF"
            opacity: 0.4
        }

        Rectangle {
            anchors.top: parent.top
            anchors.bottom: parent.bottom
            anchors.right: parent.right
            width: 2
            color: "#8EC9FF"
            opacity: 0.4
        }
    }

    MouseArea {
        id: leftMa

        anchors.top: parent.top
        anchors.bottom: parent.bottom

        visible: false
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
                root._onSelectionDraging(false)
            }
        }

        onReleased: {
            root._onSelectionDraging(true)
            leftMa.x = selRect.x
            leftMa.cursorShape = Qt.SizeHorCursor
        }
    }

    MouseArea {
        id: rightMa

        anchors.top: parent.top
        anchors.bottom: parent.bottom

        visible: false
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
                root._onSelectionDraging(false)
            }
        }

        onReleased: {
            root._onSelectionDraging(true)
            rightMa.x = selRect.x + selRect.width - rightMa.width
            rightMa.cursorShape = Qt.SizeHorCursor
        }
    }
}
