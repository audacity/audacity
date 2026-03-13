import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene
import Audacity.Spectrogram

Item {
    id: root

    required property int trackId
    required property bool isStereo
    required property real channelHeightRatio
    required property real cursorYPos

    Column {
        anchors.fill: parent

        SpectrogramChannelRuler {
            width: root.width
            height: root.isStereo ? root.height * root.channelHeightRatio : root.height

            trackId: root.trackId
            cursorYPos: root.mapToItem(this, 0, root.cursorYPos).y
        }

        SeparatorLine {
            id: separatorLine
            width: root.width
            visible: root.isStereo
        }

        SpectrogramChannelRuler {
            width: root.width
            height: root.height * (1 - root.channelHeightRatio) - separatorLine.height

            visible: root.isStereo
            trackId: root.trackId
            cursorYPos: root.mapToItem(this, 0, root.cursorYPos).y
        }
    }
}
