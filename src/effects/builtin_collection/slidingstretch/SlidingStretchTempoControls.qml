import QtQuick
import Muse.UiComponents

Row {
    id: root

    required property NavigationPanel navPanel
    required property string navigationPrefix
    property int valueFieldWidth: 64

    spacing: 16

    StyledSlider {
        anchors.verticalCenter: parent.verticalCenter
        width: parent.width - root.valueFieldWidth - root.spacing

        navigation.panel: root.navPanel
        navigation.order: 0
        navigation.name: root.navigationPrefix + "ChangeSlider"

        from: -90
        to: 500
        value: -90
        stepSize: 1
    }

    IncrementalPropertyControl {
        width: root.valueFieldWidth

        navigation.panel: root.navPanel
        navigation.order: 1
        navigation.name: root.navigationPrefix + "ChangeValue"

        minValue: -90
        maxValue: 500
        decimals: 0
        step: 1
        measureUnitsSymbol: "%"
        currentValue: -90
    }
}
