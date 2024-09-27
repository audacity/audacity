import QtQuick

Item {
    id: root

    property bool isDataSelected: false
    property alias active: selRect.visible
    property var context: null
    property real minSelection: 12 // px  4left + 4 + 4right

    signal selectionDraged(var x1, var x2, var completed)

    Rectangle {
        id: selRect

        x: root.context.selectionStartPosition
        width: root.context.selectionEndPosition - selRect.x

        anchors.top: parent.top
        anchors.bottom: parent.bottom

        visible: isDataSelected
        color: "#8EC9FF"
        opacity: 0.1

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

        x: selRect.x

        anchors.top: parent.top
        anchors.bottom: parent.bottom

        visible: isDataSelected
        width: 4
        cursorShape: Qt.SizeHorCursor

        property real startX: 0
        property real startW: 0

        onPressed: function(mouse) {
            leftMa.cursorShape = Qt.ArrowCursor
            leftMa.startX = selRect.x
            leftMa.startW = selRect.width
            leftMa.x = selRect.x
        }

        onPositionChanged: function(mouse) {
            var newWidth = leftMa.startW + (mouse.x * -1)
            if (newWidth > root.minSelection) {
                root.selectionDraged(leftMa.startX + mouse.x, leftMa.startX + mouse.x + newWidth, false)
            }
        }

        onReleased: {
            root.selectionDraged(selRect.x, selRect.x + selRect.width, true)
            leftMa.x = Qt.binding(function() { return selRect.x })
            leftMa.cursorShape = Qt.SizeHorCursor
        }
    }

    MouseArea {
        id: rightMa

        x: selRect.x + selRect.width - rightMa.width

        anchors.top: parent.top
        anchors.bottom: parent.bottom

        visible: isDataSelected
        width: 4
        cursorShape: Qt.SizeHorCursor

        property real startX: 0
        property real startW: 0

        onPressed: function(mouse) {
            rightMa.cursorShape = Qt.ArrowCursor
            rightMa.startW = selRect.width
            rightMa.x = selRect.x + selRect.width - rightMa.width
        }

        onPositionChanged: function(mouse) {
            var newWidth = rightMa.startW + mouse.x
            if (newWidth > root.minSelection) {
                root.selectionDraged(selRect.x, selRect.x + newWidth, false)
            }
        }

        onReleased: {
            root.selectionDraged(selRect.x, selRect.x + selRect.width, true)
            rightMa.x = Qt.binding(function() {return selRect.x + selRect.width - rightMa.width })
            rightMa.cursorShape = Qt.SizeHorCursor
        }
    }
}
