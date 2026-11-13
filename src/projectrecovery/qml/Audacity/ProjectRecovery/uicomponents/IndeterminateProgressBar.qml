import QtQuick

import Muse.Ui

Rectangle {
    id: root

    property bool running: visible

    implicitWidth: 240
    implicitHeight: 4

    radius: height / 2
    color: ui.theme.extra["progress_track_color"]
    clip: true

    Rectangle {
        id: segment

        width: root.width * 0.4
        height: root.height
        radius: root.radius
        color: ui.theme.extra["progress_indicator_color"]

        NumberAnimation on x {
            running: root.running
            from: -segment.width
            to: root.width
            duration: 1200
            loops: Animation.Infinite
        }
    }
}
