import QtQuick

import Muse.Ui

Rectangle {
    width: 2
    color: "transparent"

    Rectangle {
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        anchors.left: parent.left

        width: 1
        color: ui.theme.extra["white_color"]
        opacity: 0.5
    }

    Rectangle {
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        anchors.right: parent.right

        width: 1
        color: ui.theme.extra["black_color"]
    }
}
