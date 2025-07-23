import QtQuick
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects

import "../common"

EffectBase {
    id: root

    property string title: qsTrc("effects/noisereduction", "Amplify")
    property bool isApplyAllowed: noiseReduction.isApplyAllowed

    width: row.width
    implicitHeight: column.height

    model: noiseReduction

    NoiseReductionViewModel {
        id: noiseReduction

        instanceId: root.instanceId
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
                radius: 4
                width: 211
                height: getNoiseProfileBox.height

                ColumnLayout {
                    id: getNoiseProfileBox
                    width: parent.width

                    Text {
                        text: qsTrc("effects/noisereduction", "Step 1")
                        font.family: ui.theme.bodyFont.family
                        font.pixelSize: ui.theme.bodyFont.pixelSize
                        font.bold: true
                        color: ui.theme.fontPrimaryColor
                        Layout.fillWidth: true
                        Layout.margins: 16
                    }

                    Text {
                        text: qsTrc("effects/noisereduction", "Select a few seconds of isolated noise so Audacity knows what to filter out, then click Get noise profile.")
                        font.family: ui.theme.bodyFont.family
                        font.pixelSize: ui.theme.bodyFont.pixelSize
                        wrapMode: Text.Wrap
                        Layout.fillWidth: true
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16
                    }

                    FlatButton {
                        id: getNoiseProfileButton
                        text: qsTrc("effects/noisereduction", "Get noise profile")
                        onClicked: {
                            if (noiseReduction.getNoiseProfile()) {
                                root.dialogView.reject()
                            }
                        }
                        height: 28
                        Layout.fillWidth: true
                        Layout.margins: 16
                    }
                }
            }

            RoundedRectangle {
                color: ui.theme.backgroundSecondaryColor
                radius: 4
                width: 301
                height: noiseReductionColumn.height

                ColumnLayout {
                    id: noiseReductionColumn
                    width: parent.width

                    Text {
                        text: qsTrc("effects/noisereduction", "Step 2")
                        font.family: ui.theme.bodyFont.family
                        font.pixelSize: ui.theme.bodyFont.pixelSize
                        font.bold: true
                        color: ui.theme.fontPrimaryColor
                        Layout.fillWidth: true
                        Layout.margins: 16
                    }

                    StyledTextLabel {
                        text: qsTrc("effects/noisereduction", "Noise reduction")
                        horizontalAlignment: Text.AlignLeft
                        Layout.fillWidth: true
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16
                    }

                    NoiseReductionSlider {
                        Layout.fillWidth: true
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16
                        Layout.bottomMargin: 16

                        value: noiseReduction.reduction
                        measureUnitsSymbol: qsTrc("global", "dB")
                        from: 0
                        to: 48
                        isInt: true
                    }

                    StyledTextLabel {
                        text: qsTrc("effects/noisereduction", "Sensitivity")
                        horizontalAlignment: Text.AlignLeft
                        Layout.fillWidth: true
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16
                    }

                    NoiseReductionSlider {
                        Layout.fillWidth: true
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16
                        Layout.bottomMargin: 16

                        value: noiseReduction.sensitivity
                        from: 0.01
                        to: 24
                        isInt: false
                    }

                    StyledTextLabel {
                        text: qsTrc("effects/noisereduction", "Freequency smoothing")
                        horizontalAlignment: Text.AlignLeft
                        Layout.fillWidth: true
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16
                    }

                    NoiseReductionSlider {
                        Layout.fillWidth: true
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16
                        Layout.bottomMargin: 16

                        value: noiseReduction.frequencySmoothingBands
                        measureUnitsSymbol: qsTrc("effects/noisereduction", "bands")
                        from: 0
                        to: 12
                        isInt: true
                    }

                    SeparatorLine {
                        Layout.fillWidth: true
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16
                        Layout.bottomMargin: 16
                    }

                    StyledTextLabel {
                        text: qsTrc("effects/noisereduction", "Output")
                        horizontalAlignment: Text.AlignLeft
                        Layout.fillWidth: true
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16
                        Layout.bottomMargin: 10
                    }

                    RadioButtonGroup {
                        orientation: Qt.Vertical

                        Layout.fillWidth: true
                        Layout.preferredHeight: radioButtonColumn.height
                        Layout.leftMargin: 16
                        Layout.rightMargin: 16
                        Layout.bottomMargin: 16

                        Column {
                            id: radioButtonColumn
                            width: parent.width
                            spacing: 8

                            RoundedRadioButton {
                                width: parent.width

                                checked: noiseReduction.reductionMode === 0
                                text: qsTrc("effects/noisereduction", "Audio with noise removed")

                                onToggled: {
                                    noiseReduction.reductionMode = 0
                                }
                            }

                            RoundedRadioButton {
                                width: parent.width

                                checked: noiseReduction.noiseReductionMode === 1
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
