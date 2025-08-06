import QtQuick
import Muse.UiComponents
import Audacity.Effects

import "../common"

EffectBase {
    id: root

    property string title: qsTrc("effects/clickremoval", "Click Removal")
    property bool isApplyAllowed: true

    width: 328
    implicitHeight: column.height

    model: clickRemoval

    ClickRemovalViewModel {
        id: clickRemoval

        instanceId: root.instanceId
    }

    Component.onCompleted: {
        clickRemoval.init()
    }

    Column {
        id: column

        height: implicitHeight
        width: parent.width
        spacing: 16

        SliderWithTextInput {
            id: slider

            width: parent.width
            text: qsTrc("effects/clickremoval", "Threshold (lower is more sensitive)")
            value: clickRemoval.threshold
            from: clickRemoval.thresholdMin
            to: clickRemoval.thresholdMax
            step: clickRemoval.thresholdStep
            decimals: 0

            onNewValueRequested: function(newValue) {
                clickRemoval.threshold = newValue
            }
        }

        SliderWithTextInput {

            width: parent.width
            text: qsTrc("effects/clickRemoval", "Max spike width (higher is more sensitive)")
            value: clickRemoval.width
            from: clickRemoval.widthMin
            to: clickRemoval.widthMax
            step: clickRemoval.widthStep
            decimals: 0

            onNewValueRequested: function(newValue) {
                clickRemoval.width = newValue
            }
        }
    }
}
