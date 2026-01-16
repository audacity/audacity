import QtQuick

Item {
    id: root

    required property bool isDataSelected
    required property bool selectionInProgress
    required property bool spectralSelectionEnabled
    required property var pressedSpectrogram
    required property int trackId
    required property double spectralTopY
    required property double spectralBottomY
    required property double selectionStartX
    required property double selectionEndX

    signal spectralSelectionDragged(double y1, double y2, bool completed)

    //! NOTE: sync with SelectionViewController's MIN_SELECTION_PX
    property real minSelection: 1

    readonly property bool hasSpectralSelection: root.spectralSelectionEnabled && 
                                                   root.isDataSelected &&
                                                   root.spectralTopY >= 0 &&
                                                   root.spectralBottomY >= 0 &&
                                                   root.pressedSpectrogram.trackId === root.trackId

    // Top edge handle
    MouseArea {
        id: topEdge

        x: root.selectionStartX
        width: root.selectionEndX - x
        y: root.spectralTopY
        height: 8

        visible: root.hasSpectralSelection && !root.selectionInProgress
        cursorShape: Qt.SizeVerCursor
        acceptedButtons: Qt.LeftButton

        property double startY: 0
        property double startBottom: 0

        onPressed: function (mouse) {
            if (mouse.button !== Qt.LeftButton) {
                return
            }
            startY = topEdge.y
            startBottom = root.spectralBottomY
        }

        onPositionChanged: function (mouse) {
            if (!(topEdge.pressedButtons & Qt.LeftButton)) {
                return
            }
            let newY = startY + mouse.y
            let newHeight = startBottom - newY
            if (newHeight < root.minSelection) {
                newY = startBottom - root.minSelection
            }
            root.spectralSelectionDragged(newY, startBottom, false)
        }

        onReleased: function (mouse) {
            if (mouse.button !== Qt.LeftButton) {
                return
            }
            root.spectralSelectionDragged(topEdge.y, root.spectralBottomY, true)
        }
    }

    // Bottom edge handle
    MouseArea {
        id: bottomEdge

        x: root.selectionStartX
        width: root.selectionEndX - x
        y: root.spectralBottomY - 8
        height: 8

        visible: root.hasSpectralSelection && !root.selectionInProgress
        cursorShape: Qt.SizeVerCursor
        acceptedButtons: Qt.LeftButton

        property double startY: 0
        property double startTop: 0

        onPressed: function (mouse) {
            if (mouse.button !== Qt.LeftButton) {
                return
            }
            startY = bottomEdge.y
            startTop = root.spectralTopY
        }

        onPositionChanged: function (mouse) {
            if (!(bottomEdge.pressedButtons & Qt.LeftButton)) {
                return
            }
            let newY = startY + mouse.y + 8
            let newHeight = newY - startTop
            if (newHeight < root.minSelection) {
                newY = startTop + root.minSelection
            }
            root.spectralSelectionDragged(startTop, newY, false)
        }

        onReleased: function (mouse) {
            if (mouse.button !== Qt.LeftButton) {
                return
            }
            root.spectralSelectionDragged(root.spectralTopY, bottomEdge.y + 8, true)
        }
    }

    // Top-left corner
    Rectangle {
        x: root.selectionStartX - 2
        y: root.spectralTopY - 2
        width: 5
        height: 5

        visible: root.hasSpectralSelection && !root.selectionInProgress
        color: "white"
        border.color: "black"
        border.width: 1
        z: 10

        MouseArea {
            anchors.fill: parent
            acceptedButtons: Qt.LeftButton
            cursorShape: Qt.SizeFDiagCursor

            property double startY: 0
            property double startBottom: 0

            onPressed: function (mouse) {
                if (mouse.button !== Qt.LeftButton) {
                    return
                }
                startY = root.spectralTopY
                startBottom = root.spectralBottomY
            }

            onPositionChanged: function (mouse) {
                if (!(pressedButtons & Qt.LeftButton)) {
                    return
                }
                let newY = startY + mouse.y
                let newHeight = startBottom - newY
                if (newHeight < root.minSelection) {
                    newY = startBottom - root.minSelection
                }
                root.spectralSelectionDragged(newY, startBottom, false)
            }

            onReleased: function (mouse) {
                if (mouse.button !== Qt.LeftButton) {
                    return
                }
                root.spectralSelectionDragged(root.spectralTopY, root.spectralBottomY, true)
            }
        }
    }

    // Top-right corner
    Rectangle {
        x: root.selectionEndX - 3
        y: root.spectralTopY - 2
        width: 5
        height: 5

        visible: root.hasSpectralSelection && !root.selectionInProgress
        color: "white"
        border.color: "black"
        border.width: 1
        z: 10

        MouseArea {
            anchors.fill: parent
            acceptedButtons: Qt.LeftButton
            cursorShape: Qt.SizeBDiagCursor

            property double startY: 0
            property double startBottom: 0

            onPressed: function (mouse) {
                if (mouse.button !== Qt.LeftButton) {
                    return
                }
                startY = root.spectralTopY
                startBottom = root.spectralBottomY
            }

            onPositionChanged: function (mouse) {
                if (!(pressedButtons & Qt.LeftButton)) {
                    return
                }
                let newY = startY + mouse.y
                let newHeight = startBottom - newY
                if (newHeight < root.minSelection) {
                    newY = startBottom - root.minSelection
                }
                root.spectralSelectionDragged(newY, startBottom, false)
            }

            onReleased: function (mouse) {
                if (mouse.button !== Qt.LeftButton) {
                    return
                }
                root.spectralSelectionDragged(root.spectralTopY, root.spectralBottomY, true)
            }
        }
    }

    // Bottom-left corner
    Rectangle {
        x: root.selectionStartX - 2
        y: root.spectralBottomY - 3
        width: 5
        height: 5

        visible: root.hasSpectralSelection && !root.selectionInProgress
        color: "white"
        border.color: "black"
        border.width: 1
        z: 10

        MouseArea {
            anchors.fill: parent
            acceptedButtons: Qt.LeftButton
            cursorShape: Qt.SizeBDiagCursor

            property double startY: 0
            property double startTop: 0

            onPressed: function (mouse) {
                if (mouse.button !== Qt.LeftButton) {
                    return
                }
                startY = root.spectralBottomY
                startTop = root.spectralTopY
            }

            onPositionChanged: function (mouse) {
                if (!(pressedButtons & Qt.LeftButton)) {
                    return
                }
                let newY = startY + mouse.y
                let newHeight = newY - startTop
                if (newHeight < root.minSelection) {
                    newY = startTop + root.minSelection
                }
                root.spectralSelectionDragged(startTop, newY, false)
            }

            onReleased: function (mouse) {
                if (mouse.button !== Qt.LeftButton) {
                    return
                }
                root.spectralSelectionDragged(root.spectralTopY, root.spectralBottomY, true)
            }
        }
    }

    // Bottom-right corner
    Rectangle {
        x: root.selectionEndX - 3
        y: root.spectralBottomY - 3
        width: 5
        height: 5

        visible: root.hasSpectralSelection && !root.selectionInProgress
        color: "white"
        border.color: "black"
        border.width: 1
        z: 10

        MouseArea {
            anchors.fill: parent
            acceptedButtons: Qt.LeftButton
            cursorShape: Qt.SizeFDiagCursor

            property double startY: 0
            property double startTop: 0

            onPressed: function (mouse) {
                if (mouse.button !== Qt.LeftButton) {
                    return
                }
                startY = root.spectralBottomY
                startTop = root.spectralTopY
            }

            onPositionChanged: function (mouse) {
                if (!(pressedButtons & Qt.LeftButton)) {
                    return
                }
                let newY = startY + mouse.y
                let newHeight = newY - startTop
                if (newHeight < root.minSelection) {
                    newY = startTop + root.minSelection
                }
                root.spectralSelectionDragged(startTop, newY, false)
            }

            onReleased: function (mouse) {
                if (mouse.button !== Qt.LeftButton) {
                    return
                }
                root.spectralSelectionDragged(root.spectralTopY, root.spectralBottomY, true)
            }
        }
    }
}
