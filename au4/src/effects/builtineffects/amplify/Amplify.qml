/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects

EffectBase {
    id: root

    width: 450

    function apply() {
        effectModel.apply()
    }

    EffectAmplify {
        id: effectModel

        onCloseRequested: {
            root.closeRequested()
        }
    }

    Row {
        anchors.horizontalCenter: parent.horizontalCenter

        spacing: 4

        StyledTextLabel {
            anchors.verticalCenter: parent.verticalCenter

            text: qsTrc("effects/amplify", "Amplification (dB):")
        }

        TextInputField {
            anchors.verticalCenter: parent.verticalCenter
            width: 56

            currentText: effectModel.amp

            validator: DoubleInputValidator {
                top: effectModel.ampMax
                bottom: effectModel.ampMin
                decimal: 4
            }

            onTextChanged: function(newTextValue) {
                effectModel.amp = newTextValue

                effectModel.onAmpTextChanged() // todo
            }
        }
    }

    StyledSlider {
        width: parent.width

        value: effectModel.ampSliderValue
        to: effectModel.ampSliderValueMax
        from: effectModel.ampSliderValueMin
        stepSize: 1

        onMoved: {
            effectModel.ampSliderValue = value

            effectModel.onAmpSliderValueChanged(value) // todo
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
            width: 56

            currentText: effectModel.newPeakAmp

            validator: DoubleInputValidator {
                top: effectModel.newPeakAmpMax
                bottom: effectModel.newPeakAmpMin
                decimal: 4
            }

            onTextEdited: function(newTextValue) {
                effectModel.newPeakAmp = newTextValue

                effectModel.onNewPeakTextChanged() // todo
            }
        }
    }

    CheckBox {
        anchors.horizontalCenter: parent.horizontalCenter

        text: qsTrc("effects/amplify", "Allow clipping")
        checked: effectModel.allowCliping

        onClicked: {
            effectModel.allowCliping = !checked
        }
    }
}
