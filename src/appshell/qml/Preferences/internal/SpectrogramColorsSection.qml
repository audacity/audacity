/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.UiComponents

import Audacity.UiComponents 1.0
import Audacity.Spectrogram 1.0

SpectrogramBaseSection {
    id: root

    title: qsTrc("appshell/preferences/spectrogram", "Colors")

    required property AbstractSpectrogramSettingsModel settingsModel

    Column {
        id: mainColumn

        spacing: root.mediumSpacing

        Repeater {
            id: repeater

            model: ColorSectionParameterListModel {
                settingsModel: root.settingsModel
                columnWidth: root.prefsColumnWidth
            }

            IncrementalPropertyControlWithTitle {
                id: control

                navigation.panel: root.navigation
                navigation.order: index
                navigation.name: "ColorsIncrementalControl_" + index

                spacing: root.narrowSpacing
                controlWidth: colorControlWidth
                title: colorControlLabel
                minValue: colorControlMinValue
                maxValue: colorControlMaxValue
                measureUnitsSymbol: colorControlUnits
                decimals: 0
                step: 1

                currentValue: colorControlCurrentValue
                onValueEditingFinished: function (value) {
                    colorControlCurrentValue = value
                }
            }
        }

        ComboBoxWithTitle {
            width: parent.width

            navigation.panel: root.navigation
            navigation.order: repeater.count
            navigation.name: "ColorsSchemeComboBox"

            title: qsTrc("spectrogram/settings", "Scheme")
            spacing: root.narrowSpacing
            columnWidth: root.mediumControlWidth
            model: settingsModel.colorSchemeNames
            currentIndex: settingsModel.colorScheme

            onValueEdited: function (index) {
                settingsModel.colorScheme = index
            }
        }
    }
}
