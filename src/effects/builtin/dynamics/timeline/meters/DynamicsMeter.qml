import QtQuick 2.15
import Muse.Ui

Rectangle {
    id: root

    required property real dbMin
    required property color areaColor
    required property double currentMax
    required property double globalMax
    required property bool upwards

    signal clicked

    color: ui.theme.backgroundSecondaryColor
    border.color: ui.theme.strokeColor
    border.width: 1
    clip: true

    QtObject {
        id: prv

        function dbToY(db) {
            return db / root.dbMin * root.height
        }

        property color semiTransparentColor: Qt.rgba(areaColor.r, areaColor.g, areaColor.b, 0.5)
        property color almostTransparentColor: Qt.rgba(areaColor.r, areaColor.g, areaColor.b, 0.25)
    }

    Rectangle {
        id: globalMaxOutline

        width: root.width
        height: upwards ? root.height - y : prv.dbToY(root.globalMax)
        y: upwards ? prv.dbToY(root.globalMax) : 0

        color: prv.almostTransparentColor
        border.color: "transparent"
    }

    Rectangle {
        id: currentMaxBar

        width: root.width
        height: upwards ? root.height - y : prv.dbToY(root.currentMax)
        y: upwards ? prv.dbToY(root.currentMax) : 0

        color: root.areaColor
    }

    MouseArea {
        anchors.fill: parent
        cursorShape: Qt.PointingHandCursor
        onClicked: root.clicked()
    }
}
