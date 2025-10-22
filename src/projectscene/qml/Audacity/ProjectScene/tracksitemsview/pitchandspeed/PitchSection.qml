import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

Column {
    id: root

    property int pitch: 0

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "PitchSection"
        accessible.name: sectionTitle.text

        onActiveChanged: function(active) {
            if (active) {
                semitonesProperty.requestActiveFocus()
            }
        }
    }
    property int navigationRowStart: 0

    spacing: 12

    function requestActiveFocus(){
        semitonesProperty.requestActiveFocus()
    }

    signal incrementRequested()
    signal decrementRequested()

    signal valueChanged(var newValue)

    QtObject {
        id: prv

        property int pitchMinValue: -1200
        property int pitchMaxValue: 1200
    }

    StyledTextLabel {
        id: sectionTitle
        width: parent.width

        text: qsTrc("projectscene", "Clip pitch")
        horizontalAlignment: Text.AlignLeft
        font: ui.theme.bodyBoldFont
    }

    Row {
        width: parent.width

        spacing: 40

        PropertyView {
            id: semitonesProperty

            width: 72

            title: qsTrc("projectscene", "Semitones")

            currentValue: Math.trunc(root.pitch / 100)

            minValue: -12
            maxValue: 12
            decimals: 0

            validator: IntInputValidator {
                top: semitonesProperty.maxValue
                bottom: semitonesProperty.minValue
            }

            navigationName: title
            navigationPanel: root.navigationPanel
            navigationRowStart: root.navigationRowStart

            property int incOrDecPitchValueForApply: 0

            canIncrease: currentValue <= Math.trunc(prv.pitchMaxValue / 100)
            onIncrement: function(){
                incOrDecPitchValueForApply = root.pitch + 100
                return Math.trunc(incOrDecPitchValueForApply / 100)
            }

            canDecrease: currentValue >= Math.trunc(prv.pitchMinValue / 100)
            onDecrement: function(){
                incOrDecPitchValueForApply = root.pitch - 100
                return Math.trunc(incOrDecPitchValueForApply / 100)
            }

            onValueChanged: function(newValue){
                if (incOrDecPitchValueForApply !== 0) {
                    root.valueChanged(incOrDecPitchValueForApply)
                    incOrDecPitchValueForApply = 0
                    return
                }

                var newPitch = parseInt(newValue) * 100 + root.pitch % 100
                root.valueChanged(newPitch)
            }
        }

        PropertyView {
            id: centsProperty

            width: 72

            title: qsTrc("projectscene", "Cents")

            currentValue: root.pitch % 100
            minValue: 0
            maxValue: 100
            decimals: 0

            validator: IntInputValidator {
                top: centsProperty.maxValue
                bottom: centsProperty.minValue
            }

            navigationName: title
            navigationPanel: root.navigationPanel
            navigationRowStart: root.navigationRowStart + 1

            property int incOrDecPitchValueForApply: 0

            canIncrease: root.pitch < prv.pitchMaxValue
            onIncrement: function(){
                incOrDecPitchValueForApply = root.pitch + 1
                return Math.abs(incOrDecPitchValueForApply % 100)
            }

            canDecrease: root.pitch > prv.pitchMinValue
            onDecrement: function(){
                incOrDecPitchValueForApply = root.pitch - 1
                return incOrDecPitchValueForApply % 100
            }

            onValueChanged: function(newValue){
                if (incOrDecPitchValueForApply !== 0) {
                    root.valueChanged(incOrDecPitchValueForApply)
                    incOrDecPitchValueForApply = 0
                    return
                }

                var newPitch = Math.trunc(root.pitch / 100) * 100 + parseInt(newValue)
                root.valueChanged(newPitch)
            }
        }
    }
}
