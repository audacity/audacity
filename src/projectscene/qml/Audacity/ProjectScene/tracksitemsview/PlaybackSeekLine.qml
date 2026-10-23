import QtQuick

import Muse.Ui

Rectangle {
    id: root

    readonly property real devicePixelRatio: Screen.devicePixelRatio > 0 ? Screen.devicePixelRatio : 1
    readonly property real lineWidth: Math.max(1, Math.round(devicePixelRatio)) / devicePixelRatio

    width: 2 * lineWidth
    color: "transparent"

    Rectangle {
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        anchors.left: parent.left

        width: root.lineWidth
        color: ui.theme.extra["white_color"]
        opacity: 0.5
    }

    Rectangle {
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        anchors.right: parent.right

        width: root.lineWidth
        color: ui.theme.extra["black_color"]
    }
}
