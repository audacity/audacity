/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 2.15

import Muse.UiComponents 1.0
import Muse.Ui 1.0

Item {
    id: root

    property real volumeLevel: 0.0
    property real readableVolumeLevel: Math.round(root.volumeLevel * 10) / 10

    height: 32

    signal volumeLevelChangeRequested(var level)

    RowLayout {
        anchors.fill: parent
        anchors.margins: 6

        spacing: 4

        StyledIconLabel {
            Layout.preferredWidth: 16
            Layout.preferredHeight: Layout.preferredWidth
            Layout.alignment: Qt.AlighVCenter

            iconCode: IconCode.AUDIO
        }

        VolumeSlider {
            Layout.fillWidth: true
            Layout.preferredHeight: 18
            Layout.alignment: Qt.AlighVCenter

            onVolumeLevelMoved: function(level) {
                root.volumeLevelChangeRequested(Math.round(level * 10) / 10)
            }
        }
    }
}
