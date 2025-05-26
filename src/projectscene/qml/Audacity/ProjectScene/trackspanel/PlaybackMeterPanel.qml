/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene 1.0
import Audacity.Playback 1.0

import "audio"

Item {
    id: root

    property int meterStyle: PlaybackMeterStyle.Default
    property int meterType: PlaybackMeterType.DbLog
    property int meterPosition: PlaybackMeterPosition.TopBar

    property real leftChannelPressure: -60.0
    property real leftChannelRMS: -60.0
    property real rightChannelPressure: -60.0
    property real rightChannelRMS: -60.0

    signal positionChangeRequested(int position)
    signal styleChangeRequested(int style)
    signal typeChangeRequested(int type)

    ColumnLayout {
        anchors.fill: parent

        FlatButton {
            id: meterOptionsBtn

            Layout.preferredWidth: parent.width
            Layout.preferredHeight: 40

            icon: IconCode.AUDIO

            onClicked: {
                popup.isOpened ? popup.close() : popup.open()
            }

            PlaybackMeterCustomisePopup {
                id: popup

                Component.onCompleted: {
                    popup.meterStyle = root.meterStyle
                    popup.meterType = root.meterType
                    popup.meterPosition = root.meterPosition
                }

                onPositionChangeRequested: function (position) {
                    root.positionChangeRequested(position)
                }

                onStyleChangeRequested: function (style) {
                    root.styleChangeRequested(style)
                }

                onTypeChangeRequested: function (type) {
                    root.typeChangeRequested(type)
                }
            }
        }

        Row {
            id: meterChannelRow
            topPadding: 10

            spacing: 2

            Layout.preferredHeight: parent.height - 40
            Layout.alignment: Qt.AlignHCenter

            VolumePressureMeter {
                id: leftVolumePressure

                showClippedInfo: false

                currentVolumePressure: root.leftChannelPressure
                currentRMS: root.leftChannelRMS

                meterStyle: root.meterStyle

                height: parent.height - 10
                indicatorWidth: 6
            }

            VolumePressureMeter {
                id: rightVolumePressure

                showClippedInfo: false

                currentVolumePressure: root.rightChannelPressure
                currentRMS: root.rightChannelRMS

                meterStyle: root.meterStyle

                height: parent.height - 10
                indicatorWidth: 6
                showRuler: true
            }
        }
    }
}