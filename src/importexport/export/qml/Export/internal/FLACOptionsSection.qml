/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

ColumnLayout {
    id: root

    spacing: 10

    property var ffmpegPrefModel: null
    property int controlWidth: 156

    property NavigationSection navigationSection: null
    property int navigationOrderStart: 0
    readonly property int navigationOrderEnd: navPanel.order

    NavigationPanel {
        id: navPanel

        name: "FFmpegFlacOptions"
        direction: NavigationPanel.Horizontal
        section: root.navigationSection
        order: root.navigationOrderStart
        enabled: root.enabled && root.visible
        accessible.name: optionsLabel.text
    }

    StyledTextLabel {
        id: optionsLabel

        text: qsTrc("export", "FLAC options")

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

                    navigation.panel: navPanel
                    navigation.order: 0
                    navigation.accessible.name: qsTrc("export", "Compression %1").arg(currentValue)

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

                    navigation.panel: navPanel
                    navigation.order: 1
                    navigation.accessible.name: qsTrc("export", "LPC %1").arg(currentValue)

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

                    navigation.panel: navPanel
                    navigation.order: 2
                    navigation.accessible.name: qsTrc("export", "Min. PdO %1").arg(currentValue)

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

                    navigation.panel: navPanel
                    navigation.order: 3
                    navigation.accessible.name: qsTrc("export", "Min. PtO %1").arg(currentValue)

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
            spacing: 8

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

                    navigation.panel: navPanel
                    navigation.order: 4
                    navigation.accessible.name: qsTrc("export", "Frame %1").arg(currentValue)

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

                    navigation.panel: navPanel
                    navigation.order: 5
                    navigation.accessible.name: qsTrc("export", "PdO method %1").arg(currentText)

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

                    navigation.panel: navPanel
                    navigation.order: 6
                    navigation.accessible.name: qsTrc("export", "Max. PdO %1").arg(currentValue)

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

                    navigation.panel: navPanel
                    navigation.order: 7
                    navigation.accessible.name: qsTrc("export", "Max. PtO %1").arg(currentValue)

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

        navigation.panel: navPanel
        navigation.order: 8

        checked: ffmpegPrefModel.useLpc

        onClicked: {
            ffmpegPrefModel.setUseLpc(!checked)
        }
    }
}
