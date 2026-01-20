import QtQuick
import Muse.Ui
import Muse.UiComponents

Rectangle {
    id: root

    QtObject {
        id: prv
        property color borderColor: ui.theme.extra["black_color"]
        property color centerLineColor: ui.theme.extra["white_color"]
        property int borderWidth: 1 // can be any number
        property int centerLineWidth: 1 // must be odd number
    }

    antialiasing: true

    Rectangle {
        id: cursor

        x: -prv.borderWidth - Math.floor(prv.centerLineWidth / 2) // offset to align center line
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
