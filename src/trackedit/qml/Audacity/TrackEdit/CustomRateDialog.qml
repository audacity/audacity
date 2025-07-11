/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Preferences

StyledDialogView {
    id: root

    property int rate: 44100

    title: qsTrc("effects", "Set rate")

    QtObject {
        id: prv

        readonly property string measureUnitsSymbol: qsTrc("global", "Hz")
    }

    contentWidth: 280
    contentHeight: 80

    margins: 16

    Row {
        id: rateRow

        anchors {
            left: parent.left
            right: parent.right
            top: parent.top
        }

        spacing: 16
        
        StyledTextLabel {
            id: label

            anchors.verticalCenter: parent.verticalCenter

            text: qsTrc("trackedit/rate", "New sample rate (Hz):")
        }

        IncrementalPropertyControl {
            id: control
            width: parent.width * .4

            currentValue: root.rate

            minValue: 1
            maxValue: 1000000
            step: 1
            decimals: 0

            measureUnitsSymbol: prv.measureUnitsSymbol

            onValueEdited: function(newValue) {
                root.rate = newValue;
            }
        }        
    }

    ButtonBox {
        id: bbox
        width: parent.width
        anchors.bottom: parent.bottom

        buttons: [ ButtonBoxModel.Cancel, ButtonBoxModel.Ok]

        onStandardButtonClicked: function(buttonId) {
            switch (buttonId) {
            case ButtonBoxModel.Cancel:
                root.reject()
                return
            case ButtonBoxModel.Ok:
                root.ret = {
                    errcode: 0,
                    value: root.rate
                }
                root.hide()
                return

            }
        }
    }
}
