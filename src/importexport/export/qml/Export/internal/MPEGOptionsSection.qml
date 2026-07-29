/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Controls
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

        name: "FFmpegMpegOptions"
        direction: NavigationPanel.Horizontal
        section: root.navigationSection
        order: root.navigationOrderStart
        enabled: root.enabled && root.visible
        accessible.name: optionsLabel.text
    }

    StyledTextLabel {
        id: optionsLabel

        text: qsTrc("export", "MPEG container options")
        font.bold: true
    }

    RowLayout {
        Layout.fillWidth: true

        ColumnLayout {

            Layout.fillWidth: true

            RowLayout {

                Layout.fillWidth: true

                StyledTextLabel {
                    text: qsTrc("export", "Mux rate")
                }

                Item {
                    Layout.fillWidth: true
                }

                IncrementalPropertyControl {
                    implicitWidth: root.controlWidth

                    minValue: 0
                    maxValue: 10000000
                    decimals: 0
                    step: 1

                    currentValue: ffmpegPrefModel.muxRate

                    navigation.panel: navPanel
                    navigation.order: 0
                    navigation.accessible.name: qsTrc("export", "Mux rate %1").arg(currentValue)

                    onValueEdited: function (newValue) {
                        ffmpegPrefModel.setMuxRate(newValue)
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

            RowLayout {

                Layout.fillWidth: true

                StyledTextLabel {
                    text: qsTrc("export", "Packet size")
                }

                Item {
                    Layout.fillWidth: true
                }

                IncrementalPropertyControl {
                    implicitWidth: root.controlWidth

                    minValue: 0
                    maxValue: 10000000
                    decimals: 0
                    step: 1

                    currentValue: ffmpegPrefModel.packetSize

                    navigation.panel: navPanel
                    navigation.order: 1
                    navigation.accessible.name: qsTrc("export", "Packet size %1").arg(currentValue)

                    onValueEdited: function (newValue) {
                        ffmpegPrefModel.setPacketSize(newValue)
                    }
                }
            }
        }
    }
}
