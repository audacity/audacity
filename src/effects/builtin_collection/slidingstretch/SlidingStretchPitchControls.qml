import QtQuick
import QtQuick.Layouts
import Muse.UiComponents

Column {
    id: root

    required property NavigationPanel navPanel
    required property string navigationPrefix
    property int valueFieldWidth: 64

    spacing: 16

    RowLayout {
        width: parent.width

        StyledTextLabel {
            Layout.alignment: Qt.AlignLeft
            Layout.fillWidth: true

            text: qsTrc("effects/slidingstretch", "Semitones (-12 → 12)")
            horizontalAlignment: Text.AlignLeft
        }

        IncrementalPropertyControl {
            Layout.alignment: Qt.AlignRight
            Layout.preferredWidth: root.valueFieldWidth

            navigation.panel: root.navPanel
            navigation.order: 0
            navigation.name: root.navigationPrefix + "SemitoneChangeValue"

            minValue: -12
            maxValue: 12
            decimals: 0
            step: 1
            currentValue: -12
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
                Layout.alignment: Qt.AlignLeft
                Layout.fillWidth: true
                from: -50
                to: 100
                value: -50
                stepSize: 1

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

                minValue: -50
                maxValue: 100
                decimals: 0
                step: 1
                measureUnitsSymbol: "%"
                currentValue: -50
            }
        }
    }
}
