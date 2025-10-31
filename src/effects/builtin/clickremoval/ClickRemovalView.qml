import QtQuick
import Muse.UiComponents
import Audacity.Effects
import Audacity.BuiltinEffects

BuiltinEffectBase {
    id: root

    property string title: clickRemoval.effectTitle
    property bool isApplyAllowed: true

    width: 328
    implicitHeight: column.height

    model: clickRemoval

    ClickRemovalViewModel {
        id: clickRemoval
    }

    Column {
        id: column

        height: implicitHeight
        width: parent.width
        spacing: ui.theme.extra.spacing_xl

        SliderWithTextInput {
            id: thresholdSlider

            width: parent.width
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
