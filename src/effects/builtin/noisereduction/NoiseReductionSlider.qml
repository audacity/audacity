import QtQuick
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects

Row {
    id: root

    property alias value: slider.value
    property bool isInt: false
    property alias from: slider.from
    property alias to: slider.to
    property alias measureUnitsSymbol: textControl.measureUnitsSymbol

    QtObject {
        id: prv
        property int decimals: root.isInt ? 0 : 2
    }

    spacing: 16

    StyledSlider {
        id: slider
        width: (root.width - root.spacing) * 0.6
        anchors.verticalCenter: root.verticalCenter
        onMoved: {
            slider.value = prv.decimals === 0 ? Math.round(slider.value) : parseFloat(slider.value.toFixed(prv.decimals));
        }
    }

    IncrementalPropertyControl {
        id: textControl
        width: (root.width - root.spacing) * 0.4
        decimals: root.isInt ? 0 : 2
        minValue: root.from
        maxValue: root.to
        step: 1
        currentValue: slider.value
        onValueEdited: function(newValue) {
            if (slider.value !== newValue) {
                slider.value = newValue
            }
        }
    }
}
