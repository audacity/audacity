import QtQuick

import Muse.Ui
import Muse.UiComponents

Column {
    id: root

    property string title: ""

    property alias currentValue: spinBoxItem.currentValue
    property alias minValue: spinBoxItem.minValue
    property alias maxValue: spinBoxItem.maxValue
    property alias measureUnitsSymbol: spinBoxItem.measureUnitsSymbol
    property alias canIncrease: spinBoxItem.canIncrease
    property alias onIncrement: spinBoxItem.onIncrement
    property alias canDecrease: spinBoxItem.canDecrease
    property alias onDecrement: spinBoxItem.onDecrement

    property string navigationName: "PropertyView"
    property NavigationPanel navigationPanel: null
    property int navigationRowStart: 0

    width: parent.width

    spacing: 4

    function requestActiveFocus(){
        spinBoxItem.navigation.requestActive()
    }

    signal valueChanged(var newValue)

    StyledTextLabel {
        width: parent.width

        text: root.title
        horizontalAlignment: Text.AlignLeft
    }

    IncrementalPropertyControl {
        id: spinBoxItem

        decimals: 0
        step: 1

        validator: IntInputValidator {
            top: spinBoxItem.maxValue
            bottom: spinBoxItem.minValue
        }

        navigation.name: root.navigationName + " Spinbox"
        navigation.panel: root.navigationPanel
        navigation.row: root.navigationRowStart + 1
        navigation.accessible.name: root.title + " " + currentValue

        onValueEditingFinished: function(newValue) {
            root.valueChanged(newValue)
        }
    }
}
