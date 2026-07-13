/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.UiComponents

ColumnLayout {
    id: root

    spacing: 10

    property var ffmpegPrefModel: null
    property int controlWidth: 156
    property int maxIntValue: Math.pow(2, 31) - 1

    StyledTextLabel {
        text: qsTrc("export", "General options")

        font.bold: true
    }

    RowLayout {

        Layout.fillWidth: true

        ColumnLayout {

            Layout.fillWidth: true
            spacing: 8

            RowLayout {

                Layout.fillWidth: true

                StyledTextLabel {
                    text: qsTrc("export", "Language")
                }

                Item {
                    Layout.fillWidth: true
                }

                TextInputField {
                    implicitWidth: root.controlWidth

                    currentText: ffmpegPrefModel.language

                    navigation.accessible.name: qsTrc("export", "Language")

                    onTextChanged: function (newTextValue) {
                        ffmpegPrefModel.setLanguage(newTextValue)
                    }
                }
            }

            RowLayout {

                Layout.fillWidth: true

                StyledTextLabel {
                    text: qsTrc("export", "Tag")
                }

                Item {
                    Layout.fillWidth: true
                }

                TextInputField {
                    implicitWidth: root.controlWidth

                    currentText: ffmpegPrefModel.tag

                    navigation.accessible.name: qsTrc("export", "Tag")

                    onTextChanged: function (newTextValue) {
                        ffmpegPrefModel.setTag(newTextValue)
                    }
                }
            }

            RowLayout {
                Layout.fillWidth: true

                StyledTextLabel {
                    text: qsTrc("export", "Quality")
                }

                Item {
                    Layout.fillWidth: true
                }

                IncrementalPropertyControl {
                    implicitWidth: root.controlWidth

                    minValue: -1
                    maxValue: 500
                    decimals: 0
                    step: 1

                    currentValue: ffmpegPrefModel.quality

                    navigation.accessible.name: qsTrc("export", "Quality %1").arg(currentValue)

                    onValueEdited: function (newValue) {
                        ffmpegPrefModel.setQuality(newValue)
                    }
                }
            }

            RowLayout {

                Layout.fillWidth: true

                StyledTextLabel {
                    text: qsTrc("export", "Cutoff")
                }

                Item {
                    Layout.fillWidth: true
                }

                IncrementalPropertyControl {
                    implicitWidth: root.controlWidth

                    minValue: 0
                    maxValue: root.maxIntValue
                    decimals: 0
                    step: 1

                    currentValue: ffmpegPrefModel.cutoff

                    navigation.accessible.name: qsTrc("export", "Cutoff %1").arg(currentValue)

                    onValueEdited: function (newValue) {
                        ffmpegPrefModel.setCutoff(newValue)
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
            spacing: 8

            RowLayout {

                Layout.fillWidth: true

                StyledTextLabel {
                    text: qsTrc("export", "Bit rate")
                }

                Item {
                    Layout.fillWidth: true
                }

                IncrementalPropertyControl {
                    implicitWidth: root.controlWidth

                    minValue: 0
                    maxValue: 1000000
                    decimals: 0
                    step: 1

                    currentValue: ffmpegPrefModel.bitrate

                    navigation.accessible.name: qsTrc("export", "Bit rate %1").arg(currentValue)

                    onValueEdited: function (newValue) {
                        ffmpegPrefModel.setBitrate(newValue)
                    }

                    // TODO: add tooltips
                    ToolTip {
                        id: bitrateTooltip

                        text: qsTrc("export", "Bit rate (bits/second) - influences the resulting file size and quality\nSome codecs may only accept specific values (128k, 192k, 256k, etc.)\n0 - automatic\n192000 - recommended")
                    }
                }
            }

            RowLayout {
                StyledTextLabel {
                    text: qsTrc("export", "Sample rate")
                }

                Item {
                    Layout.fillWidth: true
                }

                IncrementalPropertyControl {
                    implicitWidth: root.controlWidth

                    minValue: 0
                    maxValue: 200000
                    decimals: 0
                    step: 1

                    currentValue: ffmpegPrefModel.sampleRate

                    navigation.accessible.name: qsTrc("export", "Sample rate %1").arg(currentValue)

                    onValueEdited: function (newValue) {
                        ffmpegPrefModel.setSampleRate(newValue)
                    }
                }
            }

            RowLayout {
                StyledTextLabel {
                    text: qsTrc("export", "Profile")
                }

                Item {
                    Layout.fillWidth: true
                }

                StyledDropdown {
                    width: root.controlWidth

                    model: ffmpegPrefModel.profileList

                    currentIndex: indexOfValue(ffmpegPrefModel.profile)

                    navigation.accessible.name: qsTrc("export", "Profile %1").arg(currentText)

                    onActivated: function (index, value) {
                        ffmpegPrefModel.setProfile(value)
                    }
                }
            }
        }
    }

    RowLayout {
        CheckBox {
            id: bitReservoirCheckbox

            text: qsTrc("appshell/preferences", "Bit reservoir")

            checked: ffmpegPrefModel.bitReservoir

            onClicked: {
                ffmpegPrefModel.setBitReservoir(!checked)
            }
        }

        CheckBox {
            id: vblCheckbox

            text: qsTrc("appshell/preferences", "VBL")

            checked: ffmpegPrefModel.vbl

            onClicked: {
                ffmpegPrefModel.setVbl(!checked)
            }
        }
    }
}
