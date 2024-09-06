import QtQuick

Item {
    id: root

    property double channelHeightRatio: 0.5
    property alias color : splitter.color
    property alias thickness: splitter.height

    opacity: 0.05

    signal ratioChanged(double v)

    QtObject {
        id: prv

        readonly property double minChannelHeight: 20
        readonly property double minRatio: minChannelHeight / root.height
        readonly property double maxRatio: (root.height - minChannelHeight) / root.height
    }

    Rectangle {
        id: splitter

        height: 1
        width: root.width
    }

    MouseArea {
        anchors.fill: splitter
        anchors.margins: -2

        drag.target: splitter
        drag.axis: Drag.YAxis
        drag.threshold: 0

        cursorShape: Qt.SplitVCursor

        onPositionChanged: {
            let ratio = splitter.y / root.height
            clampRatio(ratio)
        }
    }

    Component.onCompleted: {
        root.heightChanged.connect(function() {
            clampRatio(root.channelHeightRatio)
        });
    }

    function clampRatio(ratio) {
        let newRatio = Math.max(prv.minRatio, Math.min(prv.maxRatio, ratio));
        root.ratioChanged(newRatio)
        splitter.y = Math.round(newRatio * root.height - 1)
    }

    Binding {
        target: splitter
        property: "y"
        value: Math.round(root.channelHeightRatio * root.height - 1)
    }
}
