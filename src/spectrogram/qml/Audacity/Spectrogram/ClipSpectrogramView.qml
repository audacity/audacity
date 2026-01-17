import QtQuick 2.15
import QtQuick.Layouts 1.15

import Audacity.Spectrogram 1.0
import Audacity.ProjectScene

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
    required property bool selectionInProgress
    required property var context
    required property var selectionViewController

    function getSpectrogramHit(y /* relative to this item */) {
        const channel = y < height * channelHeightRatio ? 0 : 1
        const spectrogramY = channel === 0 ? y : y - height * channelHeightRatio
        const spectrogramHeight = channel === 0 ? height * channelHeightRatio : height * (1.0 - channelHeightRatio)
        return SpectrogramHitFactory.createSpectrogramHit(root.trackId, channel, spectrogramY, spectrogramHeight)
    }

    function timeToPosition(time) {
        return (time - root.frameStartTime) * root.zoom
    }

    ColumnLayout {
        anchors.fill: parent

        spacing: 0

        Repeater {
            model: root.isStereo ? 2 : 1

            Item {
                id: channelItem

                Layout.fillWidth: true
                Layout.preferredHeight: {
                    if (!root.isStereo) {
                        return root.height
                    }
                    return root.height * (index === 0 ? root.channelHeightRatio : (1.0 - root.channelHeightRatio))
                }

                ClipChannelSpectrogramView {
                    id: spectrogramView

                    anchors.fill: parent

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

                // Spectral selection overlay for this channel
                SpectralSelection {
                    id: spectralSelectionOverlay

                    anchors.fill: parent
                    z: 10

                    isDataSelected: root.context ? root.context.selectionActive : false
                    selectionInProgress: root.selectionInProgress
                    spectralSelectionEnabled: root.spectralSelectionEnabled
                    pressedSpectrogram: root.pressedSpectrogram
                    trackId: root.trackId
                    
                    // Convert frequencies to Y positions
                    spectralTopY: root.selectionViewController && root.selectionEndFrequency >= 0 ? 
                        root.selectionViewController.frequencyToSpectrogramY(root.trackId, root.selectionEndFrequency, channelItem.height) : -1
                    spectralBottomY: root.selectionViewController && root.selectionStartFrequency >= 0 ? 
                        root.selectionViewController.frequencyToSpectrogramY(root.trackId, root.selectionStartFrequency, channelItem.height) : -1
                    
                    // Calculate selection time bounds in position coordinates
                    selectionStartX: timeToPosition(root.selectionStartTime) - root.timelineIndentWidth
                    selectionEndX: timeToPosition(root.selectionEndTime) - root.timelineIndentWidth

                    onSpectralSelectionDragged: function(y1, y2, completed) {
                        // Y positions are relative to this channel's spectrogram
                        // Call SelectionViewController to handle the frequency update
                        if (root.selectionViewController) {
                            root.selectionViewController.onSpectralSelectionDragged(root.trackId, y1, y2, channelItem.height, completed)
                        }
                    }
                }
            }
        }
    }
}
