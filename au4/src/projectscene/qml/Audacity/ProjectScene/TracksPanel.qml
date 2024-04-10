import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Item {

    id: root

    property NavigationSection navigationSection: null
    property NavigationPanel navigationPanel: null

    Component.onCompleted: {
        tracksModel.init()
    }

    TracksListModel {
        id: tracksModel
    }

    StyledTextLabel {
        anchors.centerIn: parent
        text: "Tracks Panel"
        visible: view.count === 0
    }

    StyledListView {
        id: view
        anchors.fill: parent
        spacing: 4
        model: tracksModel
        delegate: Rectangle {
            anchors.left: parent.left
            anchors.right: parent.right
            height: 96

            border.width: 1
            border.color: ui.theme.strokeColor
            radius: 4
            color: ui.theme.backgroundSecondaryColor

            StyledTextLabel {
                property var item: itemData

                anchors.fill: parent
                anchors.margins: 8
                verticalAlignment: Text.AlignVCenter
                horizontalAlignment: Text.AlignLeft
                text: "[" + item.id + "] " + item.title
            }
        }
    }

}
