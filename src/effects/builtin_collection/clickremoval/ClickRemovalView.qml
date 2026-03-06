import QtQuick
import Muse.UiComponents
import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection

BuiltinEffectBase {
    id: root

    property string title: clickRemoval.effectTitle
    property bool isApplyAllowed: true
    property int bottomButtonsNavigationPanelOrder: 2

    width: 328
    implicitHeight: column.height

    builtinEffectModel: ClickRemovalViewModelFactory.createModel(root, root.instanceId)
    property alias clickRemoval: root.builtinEffectModel
    property NavigationPanel clickRemovalNavigationPanel: NavigationPanel {
        name: "ClickRemovalSliders"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }

    Column {
        id: column

        height: implicitHeight
        width: parent.width
        spacing: 16

        SliderWithTextInput {
            id: thresholdSlider

            width: parent.width

            navigationPanel: root.clickRemovalNavigationPanel
            navigationOrderStart: 0

            text: clickRemoval.thresholdLabel
            value: clickRemoval.thresholdValue
            from: clickRemoval.thresholdMin
            to: clickRemoval.thresholdMax
            step: clickRemoval.thresholdStep
            decimals: clickRemoval.thresholdDecimals

            onNewValueRequested: function (newValue) {
                clickRemoval.thresholdValue = newValue
            }
        }

        SliderWithTextInput {
            id: widthSlider

            width: parent.width

            navigationPanel: root.clickRemovalNavigationPanel
            navigationOrderStart: thresholdSlider.navigationOrderStart + 2

            text: clickRemoval.widthLabel
            value: clickRemoval.widthValue
            from: clickRemoval.widthMin
            to: clickRemoval.widthMax
            step: clickRemoval.widthStep
            decimals: clickRemoval.widthDecimals

            onNewValueRequested: function (newValue) {
                clickRemoval.widthValue = newValue
            }
        }
    }
}
