import QtQuick 2.15

import Audacity.Spectrogram 1.0

Item {

    property alias clipId: spectrogramView.clipId
    property alias trackId: spectrogramView.trackId
    property alias timelineIndentWidth: spectrogramView.timelineIndentWidth
    property alias channelHeightRatio: spectrogramView.channelHeightRatio
    property alias zoom: spectrogramView.zoom
    property alias frameStartTime: spectrogramView.frameStartTime
    property alias frameEndTime: spectrogramView.frameEndTime
    property alias selectionStartTime: spectrogramView.selectionStartTime
    property alias selectionEndTime: spectrogramView.selectionEndTime
    property alias spectralSelectionStartFrequency: spectrogramView.spectralSelectionStartFrequency
    property alias spectralSelectionEndFrequency: spectrogramView.spectralSelectionEndFrequency

    SpectrogramView {
        id: spectrogramView
        anchors.fill: parent
    }
}
