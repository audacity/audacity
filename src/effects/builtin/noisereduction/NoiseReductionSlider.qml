import QtQuick
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects

Row {
    id: root

    property var value
    property bool isInt: false
    property alias from: reductionSlider.from
    property alias to: reductionSlider.to
    property alias measureUnitsSymbol: textControl.measureUnitsSymbol

    QtObject {
        id: prv
        property int decimals: root.isInt ? 0 : 2
    }

    spacing: 16

    StyledSlider {
        id: reductionSlider
        width: (root.width - root.spacing) * 0.6
        anchors.verticalCenter: root.verticalCenter
        value: textControl.currentValue
        onMoved: {
            const newValue = prv.decimals === 0 ? Math.round(value) : parseFloat(value.toFixed(prv.decimals));
            if (newValue !== root.value) {
                root.value = newValue
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
                root.value = newValue
            }
        }
    }
}
