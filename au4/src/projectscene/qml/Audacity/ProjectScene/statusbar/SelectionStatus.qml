import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Item {

    width: 180

    SelectionStatusModel {
        id: selectionModel
    }

    Component.onCompleted: {
        selectionModel.init()
    }

    StyledTextLabel {
        id: label
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        anchors.left: parent.left
        width: implicitWidth
        text: qsTrc("projectscene", "Selection")
    }

    StyledTextLabel {
        id: startTimeMock
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        anchors.left: label.right
        anchors.leftMargin: 8
        width: 50
        text: selectionModel.startTime
    }

    StyledTextLabel {
        id: endTimeMock
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        anchors.left: startTimeMock.right
        anchors.leftMargin: 4
        width: 50
        text: selectionModel.endTime
    }
}
