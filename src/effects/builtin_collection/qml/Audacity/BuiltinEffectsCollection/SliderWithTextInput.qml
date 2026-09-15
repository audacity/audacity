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
    property double step: decimals <= 0 ? 1 : Math.pow(10, -decimals)

    property var navigationPanel: null
    property int navigationOrderStart: 0
    readonly property int navigationOrderEnd: incrementalPropertyControl.navigation.order

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

            navigation.panel: root.navigationPanel
            navigation.order: root.navigationOrderStart
            navigation.accessible.name: root.text

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

            navigation.panel: root.navigationPanel
            navigation.order: slider.navigation.order + 1
            navigation.accessible.name: root.text + " " + currentValue + " " + measureUnitsSymbol

            width: parent.width * .35

            minValue: root.from
            maxValue: root.to
            decimals: root.decimals
            step: root.step

            currentValue: ui.df.roundReal(slider.value, root.decimals)

            onValueEdited: function (newValue) {
                if (newValue !== root.value) {
                    root.newValueRequested(newValue)
                }
            }
        }
    }
}
