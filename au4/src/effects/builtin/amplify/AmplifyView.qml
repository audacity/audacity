import QtQuick

import Muse.UiComponents

import Audacity.Effects

import "../common"

EffectBase {

    property string title: "Amplify"
    property alias instanceId: amplify.instanceId

    property alias isApplyAllowed: amplify.isApplyAllowed

    width: 300
    height: 200

    AmplifyViewModel {
        id: amplify
    }

    Component.onCompleted: {
        amplify.init()
    }

    Column {

        anchors.fill: parent
        spacing: 16

        Row {
            anchors.horizontalCenter: parent.horizontalCenter

            spacing: 4

            StyledTextLabel {
                anchors.verticalCenter: parent.verticalCenter

                text: qsTrc("effects/amplify", "Amplification (dB):")
            }

            TextInputField {
                anchors.verticalCenter: parent.verticalCenter
                width: 80

                currentText: amplify.amp.toFixed(4)

                validator: DoubleInputValidator {
                    top: amplify.ampMax
                    bottom: amplify.ampMin
                    decimal: 4
                }

                onTextEdited: function(newTextValue) {
                    amplify.amp = parseFloat(newTextValue)
                }
            }
        }

        StyledSlider {
            width: parent.width

            value: amplify.amp
            to: amplify.ampMax
            from: amplify.ampMin
            stepSize: 0.1

            onMoved: {
                amplify.amp = value
            }
        }

        Row {
            anchors.horizontalCenter: parent.horizontalCenter

            spacing: 4

            StyledTextLabel {
                anchors.verticalCenter: parent.verticalCenter

                text: qsTrc("effects/amplify", "New Peak Amplitude (dB):")
            }

            TextInputField {
                anchors.verticalCenter: parent.verticalCenter
                width: 80

                currentText: amplify.newPeak.toFixed(4)

                validator: DoubleInputValidator {
                    top: amplify.newPeakMax
                    bottom: amplify.newPeakMin
                    decimal: 4
                }

                onTextEdited: function(newTextValue) {
                    amplify.newPeak = parseFloat(newTextValue)
                }
            }
        }

        CheckBox {
            anchors.horizontalCenter: parent.horizontalCenter

            text: qsTrc("effects/amplify", "Allow clipping")
            checked: amplify.canClip

            onClicked: {
                amplify.canClip = !checked
            }
        }
    }
}
