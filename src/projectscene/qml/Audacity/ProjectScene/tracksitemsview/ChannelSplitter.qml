import QtQuick

Item {
    id: root

    property double channelHeightRatio: 0.5
    property alias color : splitter.color
    property alias thickness: splitter.height
    property alias editable: dragArea.visible
    property bool asymmetricStereoHeightsPossible: false

    opacity: 0.05

    signal positionChangeRequested(int y)

    Rectangle {
        id: splitter

        y: Math.round(root.channelHeightRatio * root.height) - 1

        height: 1
        width: root.width
    }

    MouseArea {
        id: dragArea

        anchors.fill: splitter
        anchors.margins: -2

        drag.axis: Drag.YAxis
        drag.threshold: 0

        cursorShape: Qt.SplitVCursor

        enabled: asymmetricStereoHeightsPossible

        onPositionChanged: {
            var newY = mapToItem(root, mouse.x, mouse.y).y
            root.positionChangeRequested(newY)
        }
    }
}
