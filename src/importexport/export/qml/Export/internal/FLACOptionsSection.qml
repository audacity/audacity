/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.UiComponents

ColumnLayout {
    id: root

    spacing: 10

    property var ffmpegPrefModel: null
    property int controlWidth: 156

    StyledTextLabel {
        text: qsTrc("export", "FLAC options")

        font.bold: true
    }

    RowLayout {
        Layout.fillWidth: true

        ColumnLayout {

            Layout.fillWidth: true
            spacing: ui.theme.extra.space_8

            RowLayout {

                Layout.fillWidth: true

                StyledTextLabel {
                    text: qsTrc("export", "Compression")
                }

                Item {
                    Layout.fillWidth: true
                }

                IncrementalPropertyControl {
                    implicitWidth: root.controlWidth

                    minValue: -1
                    maxValue: 10
                    decimals: 0
                    step: 1

                    currentValue: ffmpegPrefModel.compression

                    onValueEdited: function (newValue) {
                        ffmpegPrefModel.setCompression(newValue)
                    }
                }
            }

            RowLayout {

                Layout.fillWidth: true

                StyledTextLabel {
                    text: qsTrc("export", "LPC")
                }

                Item {
                    Layout.fillWidth: true
                }

                IncrementalPropertyControl {
                    implicitWidth: root.controlWidth

                    minValue: 0
                    maxValue: 15
                    decimals: 0
                    step: 1

                    currentValue: ffmpegPrefModel.lpc

                    onValueEdited: function (newValue) {
                        ffmpegPrefModel.setLpc(newValue)
                    }
                }
            }

            RowLayout {

                Layout.fillWidth: true

                StyledTextLabel {
                    text: qsTrc("export", "Min. PdO")
                }

                Item {
                    Layout.fillWidth: true
                }

                IncrementalPropertyControl {
                    implicitWidth: root.controlWidth

                    minValue: -1
                    maxValue: 32
                    decimals: 0
                    step: 1

                    currentValue: ffmpegPrefModel.minPdO

                    onValueEdited: function (newValue) {
                        ffmpegPrefModel.setMinPdO(newValue)
                    }
                }
            }

            RowLayout {

                Layout.fillWidth: true

                StyledTextLabel {
                    text: qsTrc("export", "Min. PtO")
                }

                Item {
                    Layout.fillWidth: true
                }

                IncrementalPropertyControl {
                    implicitWidth: root.controlWidth

                    minValue: -1
                    maxValue: 8
                    decimals: 0
                    step: 1

                    currentValue: ffmpegPrefModel.minPtO

                    onValueEdited: function (newValue) {
                        ffmpegPrefModel.setMinPtO(newValue)
                    }
                }
            }
        }

        Item {
            width: 5
        }

        ColumnLayout {

            Layout.fillWidth: true
            Layout.alignment: Qt.AlignTop
            spacing: ui.theme.extra.space_8

            RowLayout {

                Layout.fillWidth: true

                StyledTextLabel {
                    text: qsTrc("export", "Frame")
                }

                Item {
                    Layout.fillWidth: true
                }

                IncrementalPropertyControl {
                    implicitWidth: root.controlWidth

                    minValue: 0
                    maxValue: 65535
                    decimals: 0
                    step: 1

                    currentValue: ffmpegPrefModel.frameSize

                    onValueEdited: function (newValue) {
                        ffmpegPrefModel.setFrameSize(newValue)
                    }
                }
            }

            RowLayout {
                StyledTextLabel {
                    text: qsTrc("export", "PdO method")
                }

                Item {
                    Layout.fillWidth: true
                }

                StyledDropdown {
                    width: root.controlWidth

                    model: ffmpegPrefModel.pdOMethodList

                    currentIndex: indexOfValue(ffmpegPrefModel.pdOMethod)

                    onActivated: function (index, value) {
                        ffmpegPrefModel.setPdOMethod(index)
                    }
                }
            }

            RowLayout {
                StyledTextLabel {
                    text: qsTrc("export", "Max. PdO")
                }

                Item {
                    Layout.fillWidth: true
                }

                IncrementalPropertyControl {
                    implicitWidth: root.controlWidth

                    minValue: -1
                    maxValue: 32
                    decimals: 0
                    step: 1

                    currentValue: ffmpegPrefModel.maxPdO

                    onValueEdited: function (newValue) {
                        ffmpegPrefModel.setMaxPdO(newValue)
                    }
                }
            }

            RowLayout {
                StyledTextLabel {
                    text: qsTrc("export", "Max. PtO")
                }

                Item {
                    Layout.fillWidth: true
                }

                IncrementalPropertyControl {
                    implicitWidth: root.controlWidth

                    minValue: -1
                    maxValue: 8
                    decimals: 0
                    step: 1

                    currentValue: ffmpegPrefModel.maxPtO

                    onValueEdited: function (newValue) {
                        ffmpegPrefModel.setMaxPtO(newValue)
                    }
                }
            }
        }
    }

    CheckBox {
        id: lpcCheckbox

        text: qsTrc("appshell/preferences", "Use LPC")

        checked: ffmpegPrefModel.useLpc

        onClicked: {
            ffmpegPrefModel.setUseLpc(!checked)
        }
    }
}
