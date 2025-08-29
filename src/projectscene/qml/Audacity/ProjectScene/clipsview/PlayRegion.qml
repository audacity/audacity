/*
* Audacity: A Digital Audio Editor
*/

import QtQuick

import Audacity.ProjectScene

Rectangle {
    id: root

    required property TimelineContext context

    property alias start: playRegionModel.start
    property alias end: playRegionModel.end
    property alias active: playRegionModel.active

    y: 0
    height: parent.height / 2

    color: if (ui.theme.isDark)
               return active ? "#9999D3" : "#555555"
           else
               return active ? "#101083" : "#888888"

    opacity: ui.theme.isDark ? 0.50 : 0.35

    function updatePosition() {
        let newX = context.timeToPosition(start)
        x = newX > 0 ? newX : 0
        width = context.timeToPosition(end) - x
    }

    onStartChanged: updatePosition()
    onEndChanged: updatePosition()

    Connections {
        target: context

        function onFrameStartTimeChanged() {
            updatePosition()
        }

        function onFrameEndTimeChanged() {
            updatePosition()
        }
    }


    PlayRegionModel {
        id: playRegionModel
    }

    MouseArea {
        anchors.verticalCenter: parent.verticalCenter

        x: -width / 2
        width: 6
        height: parent.height

        cursorShape: Qt.SizeHorCursor
        acceptedButtons: Qt.NoButton
    }

    MouseArea {
        anchors.verticalCenter: parent.verticalCenter

        x: parent.width - width / 2
        width: 6
        height: parent.height

        cursorShape: Qt.SizeHorCursor
        acceptedButtons: Qt.NoButton
    }
}
