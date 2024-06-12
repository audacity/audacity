import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Item {

    SelectionStatusModel {
        id: selectionModel
    }

    width: 180


    StyledTextLabel {
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        anchors.left: parent.left

        text: qsTrc("projectscene", "Selection")
    }

    StyledTextLabel {
        id: startTimeMock
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        anchors.right: endTimeMock.right
        anchors.rightMargin: 4

        text: selectionModel.startTime
    }

    StyledTextLabel {
        id: endTimeMock
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        anchors.right: parent.right
        anchors.rightMargin: 8

        text: selectionModel.endTime
    }
}
