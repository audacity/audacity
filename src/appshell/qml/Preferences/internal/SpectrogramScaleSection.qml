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

    title: qsTrc("appshell/preferences/spectrogram", "Scale")

    required property AbstractSpectrogramSettingsModel settingsModel

    ComboBoxWithTitle {
        width: parent.width

        navigation.panel: root.navigation
        navigation.name: "ScaleComboBox"
        navigation.order: 0

        title: qsTrc("spectrogram/preferences/settings", "Scale")
        spacing: root.narrowSpacing
        columnWidth: root.mediumControlWidth
        model: settingsModel.scaleNames
        currentIndex: settingsModel.scale

        onValueEdited: function (index) {
            settingsModel.scale = index
        }
    }

    Repeater {
        id: repeater

        model: ScaleSectionParameterListModel {
            settingsModel: root.settingsModel
            columnWidth: root.prefsColumnWidth
        }

        IncrementalPropertyControlWithTitle {
            id: control

            navigation.panel: root.navigation
            navigation.order: index + 1
            navigation.name: "ScaleIncrementalControl_" + index

            spacing: root.narrowSpacing
            controlWidth: mediumControlWidth
            title: controlLabel
            minValue: controlMinValue
            maxValue: controlMaxValue
            measureUnitsSymbol: controlUnits
            decimals: 0
            step: 1

            currentValue: controlCurrentValue
            onValueEditingFinished: function (value) {
                controlCurrentValue = value
            }
        }
    }
}
