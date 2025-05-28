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

    property int meterStyle: PlaybackMeterStyle.Default
    property int meterType: PlaybackMeterType.DbLog
    property int meterPosition: PlaybackMeterPosition.TopBar

    signal positionChangeRequested(int position)
    signal styleChangeRequested(int style)
    signal typeChangeRequested(int type)

    contentWidth: 336
    contentHeight: 248

    margins: 12

    ColumnLayout {
        anchors.fill: parent
        spacing: 12

        RowLayout {
            Layout.fillWidth: true
            Layout.preferredHeight: 124
            
            spacing: 12

            StyledGroupBox {
                Layout.fillWidth: true
                Layout.fillHeight: true

                title: qsTrc("Playback", "Meter style")

                titleSpacing: 12
                itemSpacing: 8
                itemMargin: 12

                borderWidth: 1
                boarderRadius: 2

                value: root.meterStyle

                model: [
                    {label : qsTrc("playback","Default"), value: PlaybackMeterStyle.Default},
                    {label : qsTrc("playback","RMS"), value: PlaybackMeterStyle.RMS},
                    {label : qsTrc("playback","Gradient"), value: PlaybackMeterStyle.Gradient}
                ]

                onValueChangeRequested: function(value) {
                    root.styleChangeRequested(value)
                }
            }

            StyledGroupBox {
                Layout.fillWidth: true
                Layout.fillHeight: true

                title: qsTrc("Playback", "Meter type")

                titleSpacing: 12
                itemSpacing: 8
                itemMargin: 12

                borderWidth: 1
                boarderRadius: 2

                value: root.meterType

                // TODO: Modify when dbLog and Linear (amp) are implemented
                enabled: false

                model: [
                    {label : qsTrc("playback","Logarithmic (dB)"), value: PlaybackMeterType.DbLog},
                    {label : qsTrc("playback","Linear (dB)"), value: PlaybackMeterType.DbLinear},
                    {label : qsTrc("playback","Linear (amp)"), value: PlaybackMeterType.Linear}
                ]

                onValueChangeRequested: function(value) {
                    root.typeChangeRequested(value)
                }
            }
        }

        StyledGroupBox {
            Layout.fillWidth: true
            Layout.preferredHeight: 100

            title: qsTrc("playback", "Position")

            titleSpacing: 12
            itemSpacing: 8
            itemMargin: 12

            borderWidth: 1
            boarderRadius: 2

            value: root.meterPosition

            model: [
                {label : qsTrc("playback","Top bar (horizontal)"), value: PlaybackMeterPosition.TopBar},
                {label : qsTrc("playback","Side bar (vertical)"), value: PlaybackMeterPosition.SideBar}
            ]

            onValueChangeRequested: function(value) {
                root.positionChangeRequested(value)
            }
        }
    }
}