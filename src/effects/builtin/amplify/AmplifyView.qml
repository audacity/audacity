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
            value: amplify.amp
            from: amplify.ampMin
            to: amplify.ampMax

            onNewValueRequested: function(newValue) {
                amplify.amp = newValue
            }
        }

        SliderWithTextInput {

            width: parent.width
            text: qsTrc("effects/amplify", "New peak amplitude")
            value: amplify.newPeak
            from: amplify.newPeakMin
            to: amplify.newPeakMax

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
