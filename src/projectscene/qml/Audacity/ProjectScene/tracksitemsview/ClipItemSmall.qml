import QtQuick

Rectangle {

    id: root

    property color clipColor: "#677CE4"
    property bool collapsed: false

    //radius: 4
    border.width: 1
    border.color: "#000000"

    color: ui.blendColors("#ffffff", root.clipColor, 0.9)

    Rectangle {
        id: header
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.margins: 1

        height: 20

        color: root.clipColor

        visible: !root.collapsed
    }

}
