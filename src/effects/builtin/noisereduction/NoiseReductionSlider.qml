import QtQuick
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects

Row {
    id: root

    property double value: 0
    property bool isInt: false
    property alias from: slider.from
    property alias to: slider.to
    property alias measureUnitsSymbol: textControl.measureUnitsSymbol

    signal newValueRequested(double newValue)

    onValueChanged: {
        const newValue = root.value.toFixed(textControl.decimals)
        if (root.value !== newValue) {
            newValueRequested(newValue)
        }
    }

    spacing: 16

    StyledSlider {
        id: slider
        width: (root.width - root.spacing) * 0.6
        anchors.verticalCenter: root.verticalCenter
        value: root.value
        onMoved: {
            const newValue = slider.value.toFixed(textControl.decimals)
            if (root.value !== newValue) {
                newValueRequested(newValue)
            }
        }
    }

    IncrementalPropertyControl {
        id: textControl
        width: (root.width - root.spacing) * 0.4
        decimals: root.isInt ? 0 : 2
        minValue: root.from
        maxValue: root.to
        step: 1
        currentValue: root.value
        onValueEdited: function(newValue) {
            if (root.value !== newValue) {
                newValueRequested(newValue)
            }
        }
    }
}
