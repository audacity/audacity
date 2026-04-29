import QtQuick
import QtQuick.Layouts
import Muse.UiComponents

Column {
    id: root

    required property NavigationPanel navPanel
    required property string navigationPrefix
    required property var viewModel
    required property var pitchSemitones
    required property var pitchPct
    property int valueFieldWidth: 64

    spacing: 16

    Component.onCompleted: {
        pitchSemitones.init()
        pitchPct.init()
    }

    RowLayout {
        width: parent.width

        StyledTextLabel {
            Layout.alignment: Qt.AlignLeft
            Layout.fillWidth: true

            text: qsTrc("effects/slidingstretch", "Semitones (%1 → %2)").arg(pitchSemitones.min).arg(pitchSemitones.max)
            horizontalAlignment: Text.AlignLeft
        }

        IncrementalPropertyControl {
            id: semitonesControl

            Layout.alignment: Qt.AlignRight
            Layout.preferredWidth: root.valueFieldWidth

            navigation.panel: root.navPanel
            navigation.order: 0
            navigation.name: root.navigationPrefix + "SemitoneChangeValue"

            minValue: pitchSemitones.min
            maxValue: pitchSemitones.max
            step: pitchSemitones.step
            decimals: pitchSemitones.decimals
            measureUnitsSymbol: "" // "semitones" is too long. The label already indicates the unit.
            currentValue: pitchSemitones.value
            onValueEdited: function (newValue) {
                pitchSemitones.value = newValue
                pitchPct.value = root.viewModel.semitonesToPct(newValue)
            }
        }
    }

    Column {
        width: parent.width
        spacing: 4

        StyledTextLabel {
            text: qsTrc("effects/slidingstretch", "Percentage change")
        }

        RowLayout {
            width: parent.width
            spacing: 16

            StyledSlider {
                id: percentageSlider

                Layout.alignment: Qt.AlignLeft
                Layout.fillWidth: true

                from: pitchPct.min
                to: pitchPct.max
                stepSize: pitchPct.step
                value: pitchPct.value
                onValueChanged: {
                    pitchPct.value = value
                    pitchSemitones.value = root.viewModel.pctToSemitones(value)
                }

                navigation.panel: root.navPanel
                navigation.order: 1
                navigation.name: root.navigationPrefix + "PercentageChangeSlider"
            }

            IncrementalPropertyControl {
                Layout.alignment: Qt.AlignRight
                Layout.preferredWidth: root.valueFieldWidth

                navigation.panel: root.navPanel
                navigation.order: 2
                navigation.name: root.navigationPrefix + "PercentageChangeValue"

                minValue: pitchPct.min
                maxValue: pitchPct.max
                decimals: pitchPct.decimals
                step: pitchPct.step
                measureUnitsSymbol: pitchPct.unit
                currentValue: pitchPct.value
                onValueEdited: function (newValue) {
                    pitchPct.value = newValue
                    pitchSemitones.value = root.viewModel.pctToSemitones(newValue)
                }
            }
        }
    }
}
