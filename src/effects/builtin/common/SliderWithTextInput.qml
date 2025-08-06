import QtQuick
import Muse.UiComponents
import Audacity.Effects

Column {
    id: root

    required property double value
    property alias to: slider.to
    property alias from: slider.from
    property alias text: label.text

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
            stepSize: 0.01

            onMoved: {
                if (root.value !== slider.value) {
                    root.newValueRequested(slider.value)
                }
            }
        }

        IncrementalPropertyControl {

            width: parent.width * .35

            measureUnitsSymbol: qsTrc("global", "dB")
            minValue: root.from
            maxValue: root.to
            decimals: 4
            step: 0.01

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
