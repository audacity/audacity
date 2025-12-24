/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Audacity.UiComponents 1.0

import "internal"

NumericView {
    id: root

    property alias value: bpmModel.value

    showMenu: false

    model: bpmModel

    BPMModel {
        id: bpmModel

        onValueChanged: {
            root.valueChangeRequested(value)
        }
    }

    ColumnLayout {
        Layout.preferredWidth: 16
        Layout.preferredHeight: root.height

        spacing: 1

        ArrowButton {
            id: upButton

            Layout.fillWidth: true
            Layout.fillHeight: true

            isDown: false
            bottomRightRadius: 0

            navigation.name: "BPMArrowUpButton"
            navigation.panel: root.navigation.panel
            navigation.row: root.navigation.row
            navigation.column: root.navigationColumnEnd + 1
            navigation.accessible.name: qsTrc("au/uicomponents", "Up")

            onClicked: function(mouse) {
                bpmModel.upValue()
            }
        }

        ArrowButton {
            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.alignment: Qt.AlignBottom

            isDown: true
            topRightRadius: 0

            navigation.name: "BPMArrowDownButton"
            navigation.panel: root.navigation.panel
            navigation.row: root.navigation.row
            navigation.column: upButton.navigation.column + 1
            navigation.accessible.name: qsTrc("au/uicomponents", "Down")

            onClicked: function(mouse) {
                bpmModel.downValue()
            }
        }
    }
}
