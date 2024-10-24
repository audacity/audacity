import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

Column {
    id: root

    property int speedPercentage: 0

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

        width: 72

        title: qsTrc("projectscene", "Speed")

        currentValue: root.speedPercentage
        minValue: 0
        maxValue: 1000
        measureUnitsSymbol: "%"

        navigationName: title
        navigationPanel: root.navigationPanel
        navigationRowStart: root.navigationRowStart

        onValueChanged: function(newValue){
            root.valueChanged(newValue)
        }
    }
}
