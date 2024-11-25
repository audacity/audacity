import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

Column {
    id: root

    property double speedPercentage: 0.0
    property alias canChange: speedProperty.canChange

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "SpeedSection"
        accessible.name: sectionTitle.text

        onActiveChanged: function(active) {
            if (active) {
                speedProperty.requestActiveFocus()
            }
        }
    }
    property int navigationRowStart: 0

    spacing: 12

    function requestActiveFocus(){
        speedProperty.requestActiveFocus()
    }

    signal valueChanged(var newValue)

    StyledTextLabel {
        id: sectionTitle

        width: parent.width

        text: qsTrc("projectscene", "Clip speed")
        horizontalAlignment: Text.AlignLeft
        font: ui.theme.bodyBoldFont
    }

    PropertyView {
        id: speedProperty

        width: 96

        title: qsTrc("projectscene", "Speed")

        currentValue: {
            var result = parseFloat(root.speedPercentage)
            return +result.toFixed(speedProperty.decimals)
        }

        minValue: 1.0
        maxValue: 1000.0
        measureUnitsSymbol: "%"
        decimals: 3

        navigationName: title
        navigationPanel: root.navigationPanel
        navigationRowStart: root.navigationRowStart

        onValueChanged: function(newValue){
            root.valueChanged(newValue)
        }
    }
}
