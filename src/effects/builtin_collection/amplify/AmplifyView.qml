import QtQuick
import Muse.UiComponents
import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection

BuiltinEffectBase {
    id: root

    property string title: amplify.effectTitle
    property bool isApplyAllowed: amplify.isApplyAllowed
    property int bottomButtonsNavigationPanelOrder: 2

    width: 320
    implicitHeight: column.height

    builtinEffectModel: {
        var model = AmplifyViewModelFactory.createModel(root, root.instanceId)
        model.ampValueChanged.connect(function () {
            ampSlider.value = model.ampValue
        })
        return model
    }
    property alias amplify: root.builtinEffectModel
    property NavigationPanel amplifyNavigationPanel: NavigationPanel {
        name: "AmplifyControls"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }

    Component.onCompleted: {
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

            navigationPanel: root.amplifyNavigationPanel
            navigationOrderStart: 0

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

            navigationPanel: root.amplifyNavigationPanel
            navigationOrderStart: ampSlider.navigationOrderStart + 2

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
            id: canClipCheckbox

            navigation.panel: root.amplifyNavigationPanel
            navigation.order: newPeakSlider.navigationOrderStart + 2

            text: amplify.canClipLabel
            checked: amplify.canClip

            onClicked: {
                amplify.canClip = !checked
            }
        }
    }
}
