/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15
import QtQuick.Controls 2.15

import Muse.UiComponents 1.0
import Muse.Ui 1.0

import Audacity.Playback 1.0

StyledPopupView {
    id: root

    property var model: null

    contentWidth: 360
    contentHeight: 310

    margins: 12

    ColumnLayout {
        anchors.fill: parent
        spacing: 12

        StyledGroupBox {
            Layout.fillWidth: true
            Layout.preferredHeight: 92

            title: qsTrc("playback", "Position")

            titleSpacing: 0

            value: root.model.meterPosition

            model: [
                {label : qsTrc("playback","Top bar (horizontal)"), value: PlaybackMeterPosition.TopBar},
                {label : qsTrc("playback","Side bar (vertical)"), value: PlaybackMeterPosition.SideBar}
            ]

            onValueChangeRequested: function(value) {
                root.model.meterPosition = value
            }
        }

        RowLayout {
            Layout.fillWidth: true
            Layout.preferredHeight: 120
            
            spacing: 12

            StyledGroupBox {
                Layout.fillWidth: true
                Layout.fillHeight: true

                title: qsTrc("Playback", "Meter style")

                titleSpacing: 0

                value: root.model.meterStyle

                model: [
                    {label : qsTrc("playback","Default"), value: PlaybackMeterStyle.Default},
                    {label : qsTrc("playback","RMS"), value: PlaybackMeterStyle.RMS},
                    {label : qsTrc("playback","Gradient"), value: PlaybackMeterStyle.Gradient}
                ]

                onValueChangeRequested: function(value) {
                    root.model.meterStyle = value
                }
            }

            StyledGroupBox {
                Layout.fillWidth: true
                Layout.fillHeight: true

                title: qsTrc("Playback", "Meter type")

                titleSpacing: 0

                value: root.model.meterType

                model: [
                    {label : qsTrc("playback","Logarithmic (dB)"), value: PlaybackMeterType.DbLog},
                    {label : qsTrc("playback","Linear (dB)"), value: PlaybackMeterType.DbLinear},
                    {label : qsTrc("playback","Linear (amp)"), value: PlaybackMeterType.Linear}
                ]

                onValueChangeRequested: function(value) {
                    root.model.meterType = value
                }
            }
        }

        ColumnLayout {
            id: dbRangeSection

            Layout.fillWidth: true
            Layout.preferredHeight: 50

            StyledTextLabel {
                text: qsTrc("playback", "dB range")
                horizontalAlignment: Text.AlignLeft
                wrapMode: Text.WordWrap
            }

            StyledDropdown {
                id: dbRangeDropdown

                Layout.fillWidth: true
                indeterminateText: ""

                currentIndex: root.model.meterDbRange
                model: [
                    "-36 dB (shallow range for high-amplitude editing)",
                    "-48 dB (PCM range of 8 bit samples)", 
                    "-60 dB (PCM range of 10 bit samples)",
                    "-72 dB (PCM range of 12 bit samples)",
                    "-84 dB (PCM range of 14 bit samples)",
                    "-96 dB (PCM range of 16 bit samples)",
                    "-120 dB (approximate limit of human hearing)",
                    "-145 dB (PCM range of 24 bit samples)"
                ]

                onActivated: function(newIndex, newValue) {
                    root.model.meterDbRange = newIndex;
                }
            }
        }
    }
}
