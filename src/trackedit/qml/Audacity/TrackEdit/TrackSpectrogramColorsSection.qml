/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.UiComponents 1.0

import Audacity.Spectrogram 1.0

TrackSpectrogramBaseSection {
    id: root

    title: qsTrc("appshell/preferences/spectrogram", "Colors")

    required property TrackSpectrogramSettingsModel settingsModel

    Column {
        id: mainColumn

        spacing: root.narrowSpacing

        Repeater {
            model: ColorSectionParameterListModel {
                settingsModel: root.settingsModel
                columnWidth: root.prefsColumnWidth
            }

            Row {
                spacing: 0

                StyledTextLabel {
                    width: root.mediumControlWidth
                    anchors.verticalCenter: parent.verticalCenter

                    text: colorControlLabel
                    elide: Text.ElideNone
                    wrapMode: Text.NoWrap
                    horizontalAlignment: Text.AlignLeft
                }

                IncrementalPropertyControl {
                    width: root.mediumControlWidth

                    minValue: colorControlMinValue
                    maxValue: colorControlMaxValue
                    measureUnitsSymbol: colorControlUnits
                    decimals: 0
                    step: 1

                    currentValue: colorControlCurrentValue
                    onValueEditingFinished: function (newValue) {
                        colorControlCurrentValue = newValue
                    }
                }
            }
        }

        Row {
            spacing: 0

            StyledTextLabel {
                width: root.mediumControlWidth
                anchors.verticalCenter: parent.verticalCenter

                text: qsTrc("spectrogram/settings", "Scheme")
                elide: Text.ElideNone
                wrapMode: Text.NoWrap
                horizontalAlignment: Text.AlignLeft
            }

            StyledDropdown {
                width: root.largeControlWidth

                model: settingsModel.colorSchemeNames
                currentIndex: settingsModel.colorScheme
                onActivated: function (index, value) {
                    settingsModel.colorScheme = index
                }
            }
        }
    }
}
