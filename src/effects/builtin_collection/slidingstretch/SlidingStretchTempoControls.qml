import QtQuick
import Muse.UiComponents

Row {
    id: root

    required property NavigationPanel navPanel
    required property string navigationPrefix
    required property var tempo
    property int valueFieldWidth: 64

    spacing: 16

    Component.onCompleted: {
        tempo.init()
    }

    StyledSlider {
        anchors.verticalCenter: parent.verticalCenter
        width: parent.width - root.valueFieldWidth - root.spacing

        navigation.panel: root.navPanel
        navigation.order: 0
        navigation.name: root.navigationPrefix + "ChangeSlider"

        from: tempo.min
        to: tempo.max
        stepSize: tempo.step
        value: tempo.value
        onValueChanged: {
            tempo.value = value
        }
    }

    IncrementalPropertyControl {
        width: root.valueFieldWidth

        navigation.panel: root.navPanel
        navigation.order: 1
        navigation.name: root.navigationPrefix + "ChangeValue"

        minValue: tempo.min
        maxValue: tempo.max
        step: tempo.step
        decimals: tempo.decimals
        measureUnitsSymbol: tempo.unit
        currentValue: tempo.value
        onValueEdited: function (newValue) {
            tempo.value = newValue
        }
    }
}
