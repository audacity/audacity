import QtQuick

import Muse.UiComponents

import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection

BuiltinEffectBase {
    id: root

    property string title: deepFilterNet.effectTitle
    property bool isApplyAllowed: true

    width: 320
    implicitHeight: column.height
    numNavigationPanels: 1

    builtinEffectModel: DeepFilterNetViewModelFactory.createModel(root, root.instanceId)
    property alias deepFilterNet: root.builtinEffectModel
    property NavigationPanel controlsNavigationPanel: NavigationPanel {
        name: "DeepFilterNetControls"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }

    Column {
        id: column

        width: parent.width
        spacing: 16

        SliderWithTextInput {
            id: attenuationSlider

            width: parent.width

            navigationPanel: root.controlsNavigationPanel
            navigationOrderStart: 0

            text: qsTrc("effects/deepfilternet", "Attenuation limit")
            measureUnitsSymbol: qsTrc("global", "dB")
            value: deepFilterNet.attenuationLimit
            from: deepFilterNet.attenuationLimitMin
            to: deepFilterNet.attenuationLimitMax
            decimals: 0
            step: 1

            onNewValueRequested: function (newValue) {
                deepFilterNet.attenuationLimit = newValue
            }
        }

        SliderWithTextInput {
            width: parent.width

            navigationPanel: root.controlsNavigationPanel
            navigationOrderStart: attenuationSlider.navigationOrderEnd + 1

            text: qsTrc("effects/deepfilternet", "Mix")
            value: deepFilterNet.mix
            from: deepFilterNet.mixMin
            to: deepFilterNet.mixMax
            decimals: 2
            step: 0.01

            onNewValueRequested: function (newValue) {
                deepFilterNet.mix = newValue
            }
        }
    }
}
