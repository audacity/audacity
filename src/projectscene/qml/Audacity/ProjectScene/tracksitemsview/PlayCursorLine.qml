import QtQuick
import Muse.Ui
import Muse.UiComponents

Rectangle {
    id: root

    readonly property real devicePixelRatio: Screen.devicePixelRatio > 0 ? Screen.devicePixelRatio : 1

    QtObject {
        id: prv
        property color borderColor: ui.theme.extra["black_color"]
        property color centerLineColor: ui.theme.extra["white_color"]
        property real borderWidth: toDevicePixels(1)
        property real centerLineWidth: toDevicePixels(1)

        function toDevicePixels(width) {
            return Math.max(1, Math.round(width * root.devicePixelRatio)) / root.devicePixelRatio
        }
    }

    antialiasing: true

    Rectangle {
        id: cursor

        x: -prv.borderWidth // the center line starts at the cursor position
        width: prv.centerLineWidth + (2 * prv.borderWidth) // left border width, center line width, right border width
        height: root.height
        color: prv.centerLineColor
        antialiasing: true
        // draw left and right borders without top and bottom ones
        Rectangle {
            id: leftBorder
            color: prv.borderColor
            width: prv.borderWidth
            height: parent.height
            anchors.left: parent.left
            antialiasing: true
        }
        Rectangle {
            id: rightBorder
            color: prv.borderColor
            width: prv.borderWidth
            height: parent.height
            anchors.right: parent.right
            antialiasing: true
        }
    }
}
