import QtQuick
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects

import "../common"

BuiltinEffectBase {
    id: root

    property string title: qsTrc("effects/noisereduction", "Noise Reduction")
    property bool isApplyAllowed: noiseReduction.isApplyAllowed

    width: row.width
    implicitHeight: column.height

    model: noiseReduction

    NoiseReductionViewModel {
        id: noiseReduction
    }

    Component.onCompleted: {
        noiseReduction.init()
    }

    Column {
        id: column
        spacing: 0

        Row {
            id: row
            spacing: 16

            RoundedRectangle {
                color: ui.theme.backgroundSecondaryColor
                border.color: ui.theme.strokeColor
                border.width: 1
                radius: 4
                width: 211
                height: getNoiseProfileBox.height

                ColumnLayout {
                    id: getNoiseProfileBox
                    width: parent.width

                    StyledTextLabel {
                        Layout.fillWidth: true
                        Layout.margins: 16

                        text: qsTrc("effects/noisereduction", "Step 1")
                        horizontalAlignment: Text.AlignLeft
                        font.bold: true
                    }

                    StyledTextLabel {
                        Layout.fillWidth: true
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16

                        text: qsTrc("effects/noisereduction", "Select a few seconds of isolated noise so Audacity knows what to filter out, then click Get noise profile.")
                        wrapMode: Text.Wrap
                        horizontalAlignment: Text.AlignLeft
                    }

                    FlatButton {
                        id: getNoiseProfileButton

                        Layout.fillWidth: true
                        Layout.margins: 16

                        text: qsTrc("effects/noisereduction", "Get noise profile")
                        onClicked: {
                            noiseReduction.getNoiseProfile()
                            // The user has taken the profile of a small selection of noise.
                            // Close the dialog to make clearer that now, the audio to be processed must be selected.
                            root.dialogView.reject()
                        }
                        height: 28
                    }
                }
            }

            RoundedRectangle {
                color: ui.theme.backgroundSecondaryColor
                border.color: ui.theme.strokeColor
                border.width: 1
                radius: 4
                width: 301
                height: noiseReductionColumn.height

                ColumnLayout {
                    id: noiseReductionColumn
                    width: parent.width

                    StyledTextLabel {
                        Layout.fillWidth: true
                        Layout.margins: 16

                        text: qsTrc("effects/noisereduction", "Step 2")
                        horizontalAlignment: Text.AlignLeft
                        font.bold: true
                    }

                    StyledTextLabel {
                        Layout.fillWidth: true
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16
                        Layout.bottomMargin: 16

                        text: qsTrc("effects/noisereduction", "Select all of the audio you want filtered, choose how much noise you want filtered out, and then click “Apply” to reduce noise.")
                        wrapMode: Text.Wrap
                        horizontalAlignment: Text.AlignLeft
                    }

                    StyledTextLabel {
                        Layout.fillWidth: true
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16

                        text: qsTrc("effects/noisereduction", "Noise reduction")
                        horizontalAlignment: Text.AlignLeft
                    }

                    NoiseReductionSlider {
                        Layout.fillWidth: true
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16
                        Layout.bottomMargin: 16

                        value: noiseReduction.reduction
                        onNewValueRequested: function (newValue) {
                            noiseReduction.reduction = newValue
                        }
                        measureUnitsSymbol: qsTrc("global", "dB")
                        from: noiseReduction.reductionMin
                        to: noiseReduction.reductionMax
                        isInt: true
                    }

                    StyledTextLabel {
                        Layout.fillWidth: true
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16

                        text: qsTrc("effects/noisereduction", "Sensitivity")
                        horizontalAlignment: Text.AlignLeft
                    }

                    NoiseReductionSlider {
                        Layout.fillWidth: true
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16
                        Layout.bottomMargin: 16

                        value: noiseReduction.sensitivity
                        onNewValueRequested: function (newValue) {
                            noiseReduction.sensitivity = newValue
                        }
                        from: noiseReduction.sensitivityMin
                        to: noiseReduction.sensitivityMax
                        isInt: false
                    }

                    StyledTextLabel {
                        Layout.fillWidth: true
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16

                        text: qsTrc("effects/noisereduction", "Frequency smoothing")
                        horizontalAlignment: Text.AlignLeft
                    }

                    NoiseReductionSlider {
                        Layout.fillWidth: true
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16
                        Layout.bottomMargin: 16

                        value: noiseReduction.frequencySmoothingBands
                        onNewValueRequested: function (newValue) {
                            noiseReduction.frequencySmoothingBands = newValue
                        }
                        measureUnitsSymbol: qsTrc("effects/noisereduction", "bands")
                        from: noiseReduction.frequencySmoothingBandsMin
                        to: noiseReduction.frequencySmoothingBandsMax
                        isInt: true
                    }

                    SeparatorLine {
                        Layout.fillWidth: true
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16
                        Layout.bottomMargin: 16
                    }

                    StyledTextLabel {
                        Layout.fillWidth: true
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16
                        Layout.bottomMargin: 10

                        text: qsTrc("effects/noisereduction", "Output")
                        horizontalAlignment: Text.AlignLeft
                    }

                    RadioButtonGroup {
                        Layout.fillWidth: true
                        Layout.preferredHeight: radioButtonColumn.height
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16
                        Layout.bottomMargin: 16

                        orientation: Qt.Vertical

                        Column {
                            id: radioButtonColumn
                            width: parent.width
                            spacing: 8

                            RoundedRadioButton {
                                id: audioWithNoiseRemovedRadioButton
                                width: parent.width

                                checked: noiseReduction.reductionMode === 0
                                text: qsTrc("effects/noisereduction", "Audio with noise removed")

                                onToggled: {
                                    noiseReduction.reductionMode = 0
                                }
                            }

                            RoundedRadioButton {
                                width: parent.width

                                checked: !audioWithNoiseRemovedRadioButton.checked
                                text: qsTrc("effects/noisereduction", "Noise only")

                                onToggled: {
                                    noiseReduction.reductionMode = 1
                                }
                            }
                        }
                    }
                }
            }
        }

        Item {
            id: spacer
            width: 1
            height: 16
        }
    }
}
