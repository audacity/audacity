/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.UiComponents 1.0

import Audacity.Spectrogram 1.0

TrackSpectrogramBaseSection {
    id: root

    title: qsTrc("appshell/preferences/spectrogram", "Scale")

    required property TrackSpectrogramSettingsModel settingsModel

    Row {
        spacing: 0

        StyledTextLabel {
            width: root.mediumControlWidth
            anchors.verticalCenter: parent.verticalCenter

            text: qsTrc("spectrogram/settings", "Scale")
            elide: Text.ElideNone
            wrapMode: Text.NoWrap
            horizontalAlignment: Text.AlignLeft
        }

        StyledDropdown {
            width: root.largeControlWidth

            navigation.panel: root.navigation
            navigation.name: "ScaleComboBox"

            model: settingsModel.scaleNames
            currentIndex: settingsModel.scale
            onActivated: function (index, value) {
                settingsModel.scale = index
            }
        }
    }

    Repeater {
        id: repeater

        model: ScaleSectionParameterListModel {
            settingsModel: root.settingsModel
            columnWidth: root.prefsColumnWidth
        }

        Row {
            spacing: 0

            StyledTextLabel {
                width: root.mediumControlWidth
                anchors.verticalCenter: parent.verticalCenter

                text: controlLabel
                elide: Text.ElideNone
                wrapMode: Text.NoWrap
                horizontalAlignment: Text.AlignLeft
            }

            IncrementalPropertyControl {
                width: root.mediumControlWidth

                navigation.panel: root.navigation
                navigation.order: index + 1
                navigation.name: "ScaleIncrementalControl_" + index

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
}
