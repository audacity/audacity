/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

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
            Layout.fillWidth: true
            Layout.fillHeight: true

            isDown: false
            bottomRightRadius: 0

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

            onClicked: function(mouse) {
                bpmModel.downValue()
            }
        }
    }
}
