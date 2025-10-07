import QtQuick

Item {
    id: root

    property bool isDataSelected: false
    property bool selectionInProgress: false
    property alias active: selRect.visible
    property var context: null
    //! NOTE: sync with SelectionViewController's MIN_SELECTION_PX
    property real minSelection: 1

    signal selectionDraged(var x1, var x2, var completed)
    signal requestSelectionContextMenu(real x, real y)
    signal handleGuideline(var x, var completed)

    onSelectionInProgressChanged: {
        leftMa.cursorShape = root.selectionInProgress ? Qt.IBeamCursor : Qt.SizeHorCursor
        rightMa.cursorShape = root.selectionInProgress ? Qt.IBeamCursor : Qt.SizeHorCursor
    }

    Rectangle {
        id: selRect

        x: root.context.selectionStartPosition
        width: root.context.selectionEndPosition - selRect.x

        anchors.top: parent.top
        anchors.bottom: parent.bottom

        visible: isDataSelected
        color: "transparent"
    }

    MouseArea {
        id: centerMa

        acceptedButtons: Qt.RightButton

        anchors.left: leftMa.right
        anchors.right: rightMa.left
        anchors.top: parent.top
        anchors.bottom: parent.bottom

        visible: isDataSelected

        cursorShape: Qt.IBeamCursor

        onClicked: function(mouse) {
            let position = mapToItem(root.parent, Qt.point(mouse.x, mouse.y))
            root.requestSelectionContextMenu(position.x, position.y)
        }
    }

    MouseArea {
        id: leftMa

        acceptedButtons: Qt.LeftButton | Qt.RightButton

        x: selRect.x

        anchors.top: parent.top
        anchors.bottom: parent.bottom

        visible: isDataSelected
        width: selRect.width >= 16 ? 8 : (selRect.width / 2)
        cursorShape: Qt.SizeHorCursor

        property real startX: 0
        property real startW: 0

        onPressed: function(mouse) {
            if (mouse.button !== Qt.LeftButton) {
                return
            }
            leftMa.startX = selRect.x
            leftMa.startW = selRect.width
            leftMa.x = selRect.x
            centerMa.cursorShape = Qt.SizeHorCursor
            handleGuideline(selRect.x, false)
        }

        onPositionChanged: function(mouse) {
            if (!(leftMa.pressedButtons & Qt.LeftButton)) {
                return
            }
            var newWidth = leftMa.startW + (mouse.x * -1)
            if (newWidth < root.minSelection) {
                root.selectionDraged(selRect.x + selRect.width - root.minSelection, selRect.x + selRect.width, false)
            } else {
                root.selectionDraged(leftMa.startX + mouse.x, leftMa.startX + mouse.x + newWidth, false)
            }
            handleGuideline(selRect.x, false)
        }

        onReleased: function(mouse) {
            if (mouse.button !== Qt.LeftButton) {
                return
            }
            root.selectionDraged(selRect.x, selRect.x + selRect.width, true)
            leftMa.x = Qt.binding(function() { return selRect.x })
            leftMa.cursorShape = Qt.SizeHorCursor
            centerMa.cursorShape = Qt.IBeamCursor
            handleGuideline(selRect.x, true)
        }

        onClicked: function(mouse) {
            if (mouse.button === Qt.RightButton) {
                let position = mapToItem(root.parent, Qt.point(mouse.x, mouse.y))
                root.requestSelectionContextMenu(position.x, position.y)
            }
        }
    }

    MouseArea {
        id: rightMa

        acceptedButtons: Qt.LeftButton | Qt.RightButton

        x: selRect.x + selRect.width - rightMa.width

        anchors.top: parent.top
        anchors.bottom: parent.bottom

        visible: isDataSelected
        width: selRect.width >= 16 ? 8 : (selRect.width / 2)
        cursorShape: Qt.SizeHorCursor

        property real startX: 0
        property real startW: 0

        onPressed: function(mouse) {
            if (mouse.button !== Qt.LeftButton) {
                return
            }
            rightMa.startW = selRect.width
            rightMa.x = selRect.x + selRect.width - rightMa.width
            centerMa.cursorShape = Qt.SizeHorCursor
            handleGuideline(root.context.selectionEndPosition, false)
        }

        onPositionChanged: function(mouse) {
            if (!(rightMa.pressedButtons & Qt.LeftButton)) {
                return
            }
            var newWidth = rightMa.startW + mouse.x
            if (newWidth < root.minSelection) {
                newWidth = root.minSelection
            }
            root.selectionDraged(selRect.x, selRect.x + newWidth, false)
            handleGuideline(root.context.selectionEndPosition, false)
        }

        onReleased: function(mouse) {
            if (mouse.button !== Qt.LeftButton) {
                return
            }
            root.selectionDraged(selRect.x, selRect.x + selRect.width, true)
            rightMa.x = Qt.binding(function() {return selRect.x + selRect.width - rightMa.width })
            rightMa.cursorShape = Qt.SizeHorCursor
            centerMa.cursorShape = Qt.IBeamCursor
            handleGuideline(root.context.selectionEndPosition, true)
        }

        onClicked: function(mouse) {
            if (mouse.button === Qt.RightButton) {
                let position = mapToItem(root.parent, Qt.point(mouse.x, mouse.y))
                root.requestSelectionContextMenu(position.x, position.y)
            }
        }
    }
}
