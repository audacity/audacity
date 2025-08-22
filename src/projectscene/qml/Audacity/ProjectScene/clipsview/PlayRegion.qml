/*
* Audacity: A Digital Audio Editor
*/

import QtQuick

import Audacity.ProjectScene

Rectangle {
	id: root

	required property var context

	property alias start: playRegionModel.start
	property alias end: playRegionModel.end
	property alias active: playRegionModel.active

	x: context.timeToPosition(start) + context.frameStartTime * 0 + context.frameEndTime * 0
	width: context.timeToPosition(end) - x + context.frameStartTime * 0 + context.frameEndTime * 0

	y: 0
	height: parent.height / 2

	color:	if (ui.theme.isDark)
				return active ? "#9999D3" : "#555555"
			else
				return active ? "#101083" : "#888888"

	opacity: ui.theme.isDark ? 0.50 : 0.35

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
