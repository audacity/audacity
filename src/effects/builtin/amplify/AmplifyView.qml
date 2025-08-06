import QtQuick
import Muse.UiComponents
import Audacity.Effects

import "../common"

EffectBase {
    id: root

    property string title: qsTrc("effects/amplify", "Amplify")
    property alias isApplyAllowed: amplify.isApplyAllowed

    width: 320
    implicitHeight: column.height

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
        id: column

        height: implicitHeight
        width: parent.width
        spacing: 16

        SliderWithTextInput {
            id: slider

            width: parent.width
            text: qsTrc("effects/amplify", "Amplification")
            measureUnitsSymbol: qsTrc("global", "dB")
            value: amplify.amp
            from: amplify.ampMin
            to: amplify.ampMax
            decimals: 4
            step: 0.02

            onNewValueRequested: function(newValue) {
                amplify.amp = newValue
            }
        }

        SliderWithTextInput {

            width: parent.width
            text: qsTrc("effects/amplify", "New peak amplitude")
            measureUnitsSymbol: qsTrc("global", "dB")
            value: amplify.newPeak
            from: amplify.newPeakMin
            to: amplify.newPeakMax
            decimals: 4
            step: 0.02

            onNewValueRequested: function(newValue) {
                amplify.newPeak = newValue
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
