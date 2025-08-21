import QtQuick
import Muse.UiComponents
import Audacity.Effects

import "../common"

BuiltinEffectBase {
    id: root

    property string title: amplify.effectTitle
    property alias isApplyAllowed: amplify.isApplyAllowed

    width: 320
    implicitHeight: column.height

    model: amplify

    AmplifyViewModel {
        id: amplify

        onAmpValueChanged: ampSlider.value = ampValue
    }

    Component.onCompleted: {
        amplify.init()
        ampSlider.value = amplify.ampValue
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
            value: amplify.ampValue
            from: amplify.ampMin
            to: amplify.ampMax
            decimals: amplify.ampDecimals
            step: amplify.ampStep

            onNewValueRequested: function (newValue) {
                amplify.ampValue = newValue
            }
        }

        SliderWithTextInput {
            id: newPeakSlider

            width: parent.width
            text: amplify.newPeakLabel
            measureUnitsSymbol: amplify.newPeakMeasureUnitsSymbol
            value: amplify.newPeakValue
            from: amplify.newPeakMin
            to: amplify.newPeakMax
            decimals: amplify.newPeakDecimals
            step: amplify.newPeakStep

            onNewValueRequested: function (newValue) {
                amplify.newPeakValue = newValue
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
