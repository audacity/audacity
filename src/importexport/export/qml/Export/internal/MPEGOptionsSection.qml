/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.UiComponents

ColumnLayout {
    id: root

    spacing: ui.theme.extra.space_10

    property var ffmpegPrefModel: null
    property int controlWidth: 156

    StyledTextLabel {
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

                    onValueEdited: function (newValue) {
                        ffmpegPrefModel.setPacketSize(newValue)
                    }
                }
            }
        }
    }
}
