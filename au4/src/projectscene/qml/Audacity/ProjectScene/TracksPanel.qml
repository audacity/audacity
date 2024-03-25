import QtQuick

import MuseScore.Ui
import MuseScore.UiComponents

import Audacity.ProjectScene

Item {

    id: root

    property NavigationSection navigationSection: null
    property NavigationPanel navigationPanel: null

    Component.onCompleted: {
        tracksModel.load()
    }

    TracksListModel {
        id: tracksModel
    }

    StyledTextLabel {
        anchors.centerIn: parent
        text: "Tracks Panel"
    }

}
