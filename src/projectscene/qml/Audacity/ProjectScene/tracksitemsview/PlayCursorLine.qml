import QtQuick
import Muse.Ui
import Muse.UiComponents

Rectangle {

    id: root

    property color borderColor: "#000000"
    property int borderWidth: 1
    property bool timelinePressed: false

    Rectangle {
        id: cursor

        x: -width / 2
        width: 3
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
