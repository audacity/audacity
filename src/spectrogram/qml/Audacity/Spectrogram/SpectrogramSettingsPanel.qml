import QtQuick
import Muse.Ui
import Muse.UiComponents
import Audacity.Spectrogram

Item {
    id: root

    required property AbstractSpectrogramSettingsModel settingsModel

    height: mainColumn.height

    Column {
        id: mainColumn

        spacing: 12

        StyledTextLabel {
            text: qsTrc("spectrogram/settings", "Scale")
            font: ui.theme.bodyBoldFont
            horizontalAlignment: Text.AlignLeft
        }

        StyledTextLabel {
            text: qsTrc("spectrogram/settings", "Scale")
            horizontalAlignment: Text.AlignLeft
        }

        StyledDropdown {
            id: scaleDropdown

            model: settingsModel.scaleNames
            currentIndex: settingsModel.scale

            onActivated: function (index) {
                settingsModel.scale = index
            }
        }

        SeparatorLine {}

        StyledTextLabel {
            text: qsTrc("spectrogram/settings", "Colors")
            font: ui.theme.bodyBoldFont
            horizontalAlignment: Text.AlignLeft
        }

        Repeater {

            model: [
                {
                    "label": qsTrc("spectrogram/settings", "Gain"),
                    "property": "colorGainDb",
                    "units": "dB"
                },
                {
                    "label": qsTrc("spectrogram/settings", "Range"),
                    "property": "colorRangeDb",
                    "units": "dB",
                    "min": settingsModel.colorRangeDbMin
                },
                {
                    "label": qsTrc("spectrogram/settings", "High boost"),
                    "property": "colorHighBoostDbPerDec",
                    "units": "dB/dec",
                    "min": settingsModel.colorHighBoostDbPerDecMin,
                    "max": settingsModel.colorHighBoostDbPerDecMax
                }
            ]

            Column {
                id: colorsColumn

                spacing: 4

                StyledTextLabel {
                    text: modelData.label
                    horizontalAlignment: Text.AlignLeft
                }

                IncrementalPropertyControl {
                    id: control

                    width: 120

                    minValue: modelData.min !== undefined ? modelData.min : -999
                    maxValue: modelData.max !== undefined ? modelData.max : 999
                    step: 1
                    measureUnitsSymbol: modelData.units

                    currentValue: settingsModel[modelData.property]
                    onValueEditingFinished: function (value) {
                        settingsModel[modelData.property] = value
                    }
                }
            }
        }

        StyledTextLabel {
            text: qsTrc("spectrogram/settings", "Scheme")
            horizontalAlignment: Text.AlignLeft
        }

        StyledDropdown {
            model: settingsModel.colorSchemeNames
            currentIndex: settingsModel.colorScheme

            onActivated: function (index) {
                settingsModel.colorScheme = index
            }
        }

        SeparatorLine {}

        Repeater {
            model: [
                {
                    "label": qsTrc("spectrogram/settings", "Algorithm"),
                    "property": "algorithm",
                    "options": settingsModel.algorithmNames
                }
            ]

            Column {
                id: algorithmColumn

                spacing: 4

                StyledTextLabel {
                    text: modelData.label
                    horizontalAlignment: Text.AlignLeft
                }

                StyledDropdown {
                    model: modelData.options
                    currentIndex: settingsModel[modelData.property]

                    onActivated: function (index) {
                        settingsModel[modelData.property] = index
                    }
                }
            }
        }
    }
}
