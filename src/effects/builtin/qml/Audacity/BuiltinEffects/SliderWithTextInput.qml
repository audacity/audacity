import QtQuick
import Muse.UiComponents
import Audacity.Effects

Column {
    id: root

    required property double value
    required property int decimals
    property alias to: slider.to
    property alias from: slider.from
    property alias text: label.text
    property alias measureUnitsSymbol: incrementalPropertyControl.measureUnitsSymbol
    property double step: {
        if (decimals <= 0) {
            return 1;
        }
        const val = "0." + "0".repeat(decimals - 1) + "1";
        return parseFloat(val);
    }

    signal newValueRequested(double newValue)

    height: implicitHeight
    spacing: 8

    StyledTextLabel {
        id: label
    }

    Row {

        width: parent.width - spacing
        spacing: 16

        StyledSlider {
            id: slider

            anchors.verticalCenter: parent.verticalCenter

            width: parent.width * .65

            value: root.value
            stepSize: root.step

            onMoved: {
                if (root.value !== slider.value) {
                    root.newValueRequested(slider.value)
                }
            }
        }

        IncrementalPropertyControl {
            id: incrementalPropertyControl

            width: parent.width * .35

            minValue: root.from
            maxValue: root.to
            decimals: root.decimals
            step: root.step

            currentValue: (slider.value).toFixed(decimals)

            onValueEdited: function(newValue) {
                newValue = +(newValue.toFixed(decimals))
                if (newValue !== root.value) {
                    root.newValueRequested(newValue)
                }
            }
        }
    }
}
