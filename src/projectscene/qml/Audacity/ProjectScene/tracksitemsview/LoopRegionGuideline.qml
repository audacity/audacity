/*
* Audacity: A Digital Audio Editor
*/

import QtQuick

import Audacity.ProjectScene

Rectangle {
    id: root

    required property TimelineContext context
    required property real time
    required property real contentX

    function updatePosition() {
        x = context.timeToPosition(time) + contentX
    }

    width: 1
    color: ui.theme.extra["white_color"]
    opacity: 0.8

    Connections {
        target: root.context
        function onFrameStartTimeChanged() { root.updatePosition() }
        function onFrameEndTimeChanged() { root.updatePosition() }
    }

    onTimeChanged: updatePosition()
    onContentXChanged: updatePosition()

    Component.onCompleted: updatePosition()
}
