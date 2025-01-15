import QtQuick

import Muse.UiComponents

import Audacity.Effects

import "../common"

EffectBase {

    id: root

    property string title: "Amplify"
    property alias isApplyAllowed: amplify.isApplyAllowed

    width: 300
    height: 200

    model: amplify

    AmplifyViewModel {
        id: amplify
        instanceId: root.instanceId
        onAmpChanged: slider.value = amp
    }

    Component.onCompleted: {
        amplify.init()
        slider.value = amplify.amp
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

            RealInputField {
                anchors.verticalCenter: parent.verticalCenter
                width: 80

                min: amplify.ampMin
                max: amplify.ampMax
                decimals: 4

                currentValue: amplify.amp
                onCurrentValueChanged: {
                    amplify.amp = currentValue
                }
            }
        }

        StyledSlider {
            id: slider
            width: parent.width

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

            RealInputField {
                anchors.verticalCenter: parent.verticalCenter
                width: 80

                min: amplify.newPeakMin
                max: amplify.newPeakMax
                decimals: 4

                currentValue: amplify.newPeak
                onCurrentValueChanged: {
                    amplify.newPeak = currentValue
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
