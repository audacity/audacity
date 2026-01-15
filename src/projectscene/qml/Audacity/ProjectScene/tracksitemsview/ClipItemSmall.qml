import QtQuick

Rectangle {

    id: root

    required property color clipColor
    property bool collapsed: false

    //radius: 4
    border.width: 1
    border.color: ui.theme.extra["black_color"]

    color: ui.blendColors(ui.theme.extra["white_color"], root.clipColor, 0.9)

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
