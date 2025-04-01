import QtQuick
import Muse.UiComponents
import Audacity.Effects

import "../common"

EffectBase {
    id: root

    property string title: qsTrc("effects/amplify", "Amplify")
    property alias isApplyAllowed: amplify.isApplyAllowed

    width: 320
    height: 250

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

        height: implicitHeight
        width: parent.width
        spacing: 16

        Column {

            height: implicitHeight
            width: parent.width
            spacing: 8

            StyledTextLabel {
                text: qsTrc("effects/amplify", "Amplification")
            }

            Row {

                width: parent.width - spacing
                spacing: 16

                StyledSlider {
                    id: slider

                    anchors.verticalCenter: parent.verticalCenter

                    width: parent.width * .65

                    value: amplify.amp
                    to: amplify.ampMax
                    from: amplify.ampMin
                    stepSize: 0.01

                    onMoved: {
                        if (value !== amplify.amp) {
                            amplify.amp = value
                        }
                    }
                }

                IncrementalPropertyControl {

                    width: parent.width * .35

                    measureUnitsSymbol: qsTrc("global", "dB")
                    minValue: amplify.ampMin
                    maxValue: amplify.ampMax
                    decimals: 4
                    step: 0.01

                    currentValue: (amplify.amp).toFixed(decimals)

                    onValueEdited: function(newValue) {
                        newValue = +(newValue.toFixed(decimals))
                        if (newValue !== amplify.amp) {
                            amplify.amp = newValue
                        }
                    }
                }
            }
        }

        Column {

            height: implicitHeight
            width: parent.width
            spacing: 8

            StyledTextLabel {

                text: qsTrc("effects/amplify", "New peak amplitude")
            }

            Row {

                width: parent.width - spacing

                spacing: 16

                StyledSlider {

                    anchors.verticalCenter: parent.verticalCenter

                    width: parent.width * .65

                    value: amplify.newPeak
                    to: amplify.newPeakMax
                    from: amplify.newPeakMin
                    stepSize: 0.01

                    onMoved: {
                        if (value !== amplify.newPeak) {
                            amplify.newPeak = value
                        }
                    }
                }

                IncrementalPropertyControl {

                    width: parent.width * .35

                    measureUnitsSymbol: qsTrc("global", "dB")
                    minValue: amplify.newPeakMin
                    maxValue: amplify.newPeakMax
                    decimals: 4
                    step: 0.01

                    currentValue: +(amplify.newPeak).toFixed(decimals)

                    onValueEdited: function(newValue) {
                        newValue = +(newValue.toFixed(decimals))
                        if (newValue !== amplify.newPeak) {
                            amplify.newPeak = newValue
                        }
                    }
                }
            }
        }

        CheckBox {

            text: qsTrc("effects/amplify", "Allow clipping")
            checked: amplify.canClip

            onClicked: {
                amplify.canClip = !checked
            }
        }
    }
}
