import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

Column {
    id: root

    property bool optimizeForVoice: false

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "GeneralSection"
        accessible.name: sectionTitle.text

        onActiveChanged: function(active) {
            if (active) {
                optimizeCheckBox.requestActiveFocus()
            }
        }
    }
    property int navigationRowStart: 0

    spacing: 12

    signal valueChanged(var newValue)

    StyledTextLabel {
        id: sectionTitle

        width: parent.width

        text: qsTrc("projectscene", "General")
        horizontalAlignment: Text.AlignLeft
        font: ui.theme.bodyBoldFont
    }

    CheckBox {
        id: optimizeCheckBox

        text: qsTrc("projectscene", "Optimize for voice")

        checked: root.optimizeForVoice

        navigation.panel: root.navigationPanel
        navigation.order: 1

        onClicked: {
            root.valueChanged(!checked)
        }
    }
}
