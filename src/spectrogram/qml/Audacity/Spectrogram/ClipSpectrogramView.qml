import QtQuick 2.15
import QtQuick.Layouts 1.15

import Audacity.Spectrogram 1.0

Item {
    id: root

    required property bool spectralSelectionEnabled
    required property bool isStereo
    required property double channelHeightRatio
    required property int clipId
    required property int trackId
    required property var pressedSpectrogram
    required property int timelineIndentWidth
    required property double zoom
    required property double frameStartTime
    required property double frameEndTime
    required property double selectionStartTime
    required property double selectionEndTime
    required property double selectionStartFrequency
    required property double selectionEndFrequency

    function getSpectrogramHit(y /* relative to this item */) {
        const channel = y < height * channelHeightRatio ? 0 : 1
        const spectrogramY = channel === 0 ? y : y - height * channelHeightRatio
        const spectrogramHeight = channel === 0 ? height * channelHeightRatio : height * (1.0 - channelHeightRatio)
        return SpectrogramHitFactory.createSpectrogramHit(root.trackId, channel, spectrogramY, spectrogramHeight)
    }

    ColumnLayout {
        anchors.fill: parent

        spacing: 0

        Repeater {
            model: root.isStereo ? 2 : 1

            ClipChannelSpectrogramView {
                id: spectrogramView

                Layout.fillWidth: true
                Layout.preferredHeight: {
                    if (!root.isStereo) {
                        return root.height
                    }
                    return root.height * (index === 0 ? root.channelHeightRatio : (1.0 - root.channelHeightRatio))
                }

                clipId: root.clipId
                trackId: root.trackId
                channel: index
                timelineIndentWidth: root.timelineIndentWidth
                zoom: root.zoom
                frameStartTime: root.frameStartTime
                frameEndTime: root.frameEndTime
                selectionStartTime: root.selectionStartTime
                selectionEndTime: root.selectionEndTime
                selectionStartFrequency: root.selectionStartFrequency
                selectionEndFrequency: root.selectionEndFrequency

                MouseArea {
                    anchors.fill: parent

                    visible: spectralSelectionEnabled && (root.pressedSpectrogram.trackId === -1 || (root.pressedSpectrogram.trackId === root.trackId && root.pressedSpectrogram.channel === index))

                    cursorShape: Qt.CrossCursor
                    acceptedButtons: Qt.NoButton // Don't consume mouse events
                }
            }
        }
    }
}
