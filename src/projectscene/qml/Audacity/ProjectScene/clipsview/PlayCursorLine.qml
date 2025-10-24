import QtQuick
import Muse.Ui
import Muse.UiComponents

Rectangle {
    id: root

    property color borderColor: "#000000"
    property int borderWidth: 1
    property bool timelinePressed: false

    antialiasing: true

    Rectangle {
        id: cursor

        x: -1 // offset to align center line
        width: 3 // 1px border, 1px center line, 1px border
        height: root.height
        color: "#ffffff"

        // draw borders without top one
        Rectangle {
            id: leftBorder
            color: borderColor
            width: borderWidth
            height: parent.height
            anchors.left: parent.left
        }
        Rectangle {
            id: rightBorder
            color: borderColor
            width: borderWidth
            height: parent.height
            anchors.right: parent.right
        }
        Rectangle {
            id: bottomBorder
            color: borderColor
            width: root.width
            height: borderWidth
            anchors.bottom: parent.bottom
        }
    }
}
