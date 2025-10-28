import QtQuick
import Muse.Ui
import Muse.UiComponents

Rectangle {
    id: root

    QtObject {
        id: prv
        property color borderColor: "#000000"
        property color centerLineColor: "#ffffff"
        property int borderWidth: 1
        property int centerLineWidth: 1
    }

    antialiasing: true

    Rectangle {
        id: cursor

        x: - prv.centerLineWidth // offset to align center line
        width: prv.centerLineWidth + (2 * prv.borderWidth) // 1px left border, 1px center line, 1px left border
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
