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
    property real windowOriginX: 0

    readonly property real devicePixelRatio: Screen.devicePixelRatio > 0 ? Screen.devicePixelRatio : 1

    function updatePosition() {
        const position = context.timeToPosition(time) + windowOriginX
        x = Math.round(position * devicePixelRatio) / devicePixelRatio - windowOriginX + contentX
    }

    width: Math.max(1, Math.round(devicePixelRatio)) / devicePixelRatio
    color: ui.theme.extra["white_color"]
    opacity: 0.8

    Connections {
        target: root.context
        function onFrameStartTimeChanged() {
            root.updatePosition()
        }
        function onFrameEndTimeChanged() {
            root.updatePosition()
        }
    }

    onTimeChanged: updatePosition()
    onContentXChanged: updatePosition()
    onWindowOriginXChanged: updatePosition()
    onDevicePixelRatioChanged: updatePosition()

    Component.onCompleted: updatePosition()
}
