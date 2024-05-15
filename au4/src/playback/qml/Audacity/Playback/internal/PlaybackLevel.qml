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

    property alias leftCurrentVolumePressure: volumeSlider.leftCurrentVolumePressure
    property alias leftRecentPeak: volumeSlider.leftRecentPeak
    property alias leftMaxPeak: volumeSlider.leftMaxPeak

    property alias rightCurrentVolumePressure: volumeSlider.rightCurrentVolumePressure
    property alias rightRecentPeak: volumeSlider.rightRecentPeak
    property alias rightMaxPeak: volumeSlider.rightMaxPeak

    signal volumeLevelChangeRequested(var level)

    RowLayout {
        anchors.fill: parent

        spacing: 4

        StyledIconLabel {
            Layout.preferredWidth: parent.height
            Layout.preferredHeight: Layout.preferredWidth

            iconCode: IconCode.AUDIO
        }

        VolumeSlider {
            id: volumeSlider

            Layout.fillWidth: true
            Layout.preferredHeight: parent.height

            onVolumeLevelMoved: function(level) {
                root.volumeLevelChangeRequested(Math.round(level * 10) / 10)
            }
        }
    }
}
