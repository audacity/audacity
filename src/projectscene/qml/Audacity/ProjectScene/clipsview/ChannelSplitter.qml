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

        height: 1
        width: root.width
    }

    MouseArea {
        id: dragArea

        anchors.fill: splitter
        anchors.margins: -2

        drag.target: splitter
        drag.axis: Drag.YAxis
        drag.threshold: 0

        cursorShape: Qt.SplitVCursor

        enabled: asymmetricStereoHeightsPossible

        onPositionChanged: {
            root.positionChangeRequested(splitter.y)
        }
    }

    Binding {
        target: splitter
        property: "y"
        value: {
            if (root.height <= 0) {
                return 0
            }

            return Math.round(root.channelHeightRatio * root.height)
        }
    }
}
