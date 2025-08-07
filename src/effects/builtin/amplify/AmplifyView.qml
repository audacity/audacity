import QtQuick
import Muse.UiComponents
import Audacity.Effects

import "../common"

EffectBase {
    id: root

    property string title: amplify.title
    property alias isApplyAllowed: amplify.isApplyAllowed

    width: 320
    implicitHeight: column.height

    model: amplify

    AmplifyViewModel {
        id: amplify

        instanceId: root.instanceId

        onAmpChanged: ampSlider.value = amp
    }

    Component.onCompleted: {
        amplify.init()
        ampSlider.value = amplify.amp
    }

    Column {
        id: column

        height: implicitHeight
        width: parent.width
        spacing: 16

        SliderWithTextInput {
            id: ampSlider

            width: parent.width
            text: amplify.ampLabel
            measureUnitsSymbol: amplify.ampMeasureUnitsSymbol
            value: amplify.amp
            from: amplify.ampMin
            to: amplify.ampMax
            decimals: amplify.ampDecimals
            step: amplify.ampStep

            onNewValueRequested: function(newValue) {
                amplify.amp = newValue
            }
        }

        SliderWithTextInput {
            id: newPeakSlider

            width: parent.width
            text: amplify.newPeakLabel
            measureUnitsSymbol: amplify.newPeakMeasureUnitsSymbol
            value: amplify.newPeak
            from: amplify.newPeakMin
            to: amplify.newPeakMax
            decimals: amplify.newPeakDecimals
            step: amplify.newPeakStep

            onNewValueRequested: function(newValue) {
                amplify.newPeak = newValue
            }
        }

        CheckBox {

            text: amplify.canClipLabel
            checked: amplify.canClip

            onClicked: {
                amplify.canClip = !checked
            }
        }
    }
}
