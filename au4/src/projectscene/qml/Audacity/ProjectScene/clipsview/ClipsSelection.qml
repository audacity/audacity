import QtQuick

Item {
    id: root

    property alias active: selRect.visible
    property var context: null
    property real minSelection: 12 // px  4left + 4 + 4right

    signal selectionDraged(var x1, var x2, var completed)

    function onSelectionStarted() {
        selRect.visible = false
        leftMa.visible = false
        rightMa.visible = false
    }

    function onSelectionChanged(p1, p2) {
        selRect.x = root.context.selectionStartPosition
        selRect.width = root.context.selectionEndPosition - selRect.x

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
        opacity: 0.05

        Rectangle {
            anchors.top: parent.top
            anchors.bottom: parent.bottom
            anchors.left: parent.left
            width: 2
            color: "#8EC9FF"
            opacity: 0.3
        }

        Rectangle {
            anchors.top: parent.top
            anchors.bottom: parent.bottom
            anchors.right: parent.right
            width: 2
            color: "#8EC9FF"
            opacity: 0.3
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
            selRect.x = root.context.selectionStartPosition
            selRect.width = root.context.selectionEndPosition - selRect.x
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
            selRect.width = root.context.selectionEndPosition - selRect.x
            rightMa.x = selRect.x + selRect.width - rightMa.width
            rightMa.cursorShape = Qt.SizeHorCursor
        }
    }
}
