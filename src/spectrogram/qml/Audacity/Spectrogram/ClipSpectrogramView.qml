import QtQuick 2.15
import QtQuick.Layouts 1.15

import Audacity.Spectrogram 1.0

Item {
    id: root

    required property var canvas
    required property bool spectralSelectionEnabled

    required property bool selectionInProgress
    required property bool selectionEditInProgress
    required property bool verticalSelectionEditInProgress

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

    function update() {
        for (let i = 0; i < repeater.count; i++) {
            const item = repeater.itemAt(i)
            if (item) {
                item.update()
            }
        }
    }

    function getSpectrogramHit(y /* relative to tracks canvas */) {
        y = root.mapFromItem(root.canvas, 0, y).y
        if (y < 0 || y > height) {
            return null
        }
        const channel = y < height * channelHeightRatio ? 0 : 1
        const item = repeater.itemAt(channel)
        const spectrogramY = item.mapToItem(root.canvas, 0, 0).y
        return SpectrogramHitFactory.createSpectrogramHit(root.trackId, channel, spectrogramY, item.height)
    }

    ColumnLayout {
        anchors.fill: parent

        spacing: 0

        Repeater {
            id: repeater

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

                    visible: {
                        if (!root.spectralSelectionEnabled || root.selectionEditInProgress || root.verticalSelectionEditInProgress) {
                            return false
                        }
                        if (!root.selectionInProgress) {
                            return true
                        }
                        return root.pressedSpectrogram.trackId === root.trackId && root.pressedSpectrogram.channel === index
                    }

                    cursorShape: Qt.CrossCursor
                    acceptedButtons: Qt.NoButton // Don't consume mouse events
                }
            }
        }
    }
}
