/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.UiComponents 1.0
import Muse.Ui 1.0

Item {
    id: root

    property alias volumeLevel: volumeSlider.volumeLevel

    property alias leftCurrentVolumePressure: leftVolumePressure.currentVolumePressure
    property alias leftRecentPeak: leftVolumePressure.recentPeak
    property alias leftMaxPeak: leftVolumePressure.maxPeak

    property alias rightCurrentVolumePressure: rightVolumePressure.currentVolumePressure
    property alias rightRecentPeak: rightVolumePressure.recentPeak
    property alias rightMaxPeak: rightVolumePressure.maxPeak

    signal volumeLevelChangeRequested(var level)

    RowLayout {
        anchors.fill: parent

        StyledIconLabel {
            Layout.preferredWidth: parent.height
            Layout.preferredHeight: Layout.preferredWidth

            iconCode: IconCode.AUDIO
        }

        Item {
            Layout.fillWidth: true
            Layout.preferredHeight: parent.height

            Column {
                id: volumePressureContainer

                anchors.fill: parent
                anchors.margins: 4

                spacing: 2

                VolumePressureMeter {
                    id: leftVolumePressure
                }
                VolumePressureMeter {
                    id: rightVolumePressure
                    showRuler: true
                }
            }

            VolumeSlider {
                id: volumeSlider

                anchors.left: parent.left
                anchors.right: parent.right
                anchors.top: parent.top
                anchors.topMargin: 3

                onVolumeLevelMoved: function(level) {
                    root.volumeLevelChangeRequested(Math.round(level * 10) / 10)
                }
            }
        }
    }
}
