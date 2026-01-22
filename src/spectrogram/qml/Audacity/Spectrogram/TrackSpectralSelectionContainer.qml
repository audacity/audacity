import QtQuick
import Audacity.Spectrogram

Item {
    id: root

    required property var canvas
    required property int trackId
    required property real sampleRate
    required property real selectionStartPosition
    required property real selectionEndPosition
    required property real selectionStartFrequency
    required property real selectionEndFrequency
    required property real channelHeightRatio
    required property bool isStereo
    required property var selectionController

    signal selectionHorizontalResize(real x1, real x2, bool completed)

    ChannelSpectralSelectionContainer {
        id: leftOrMonoContainer

        y: 0
        height: root.height * (root.isStereo ? root.channelHeightRatio : 1)
        anchors.left: parent.left
        anchors.right: parent.right

        canvas: root.canvas
        trackId: root.trackId
        channel: 0
        trackSampleRate: root.sampleRate
        selectionStartPosition: root.selectionStartPosition
        selectionEndPosition: root.selectionEndPosition
        selectionStartFrequency: root.selectionStartFrequency
        selectionEndFrequency: root.selectionEndFrequency
        selectionController: root.selectionController

        onSelectionHorizontalResize: function (x1, x2, completed) {
            root.selectionHorizontalResize(x1, x2, completed)
        }
    }

    ChannelSpectralSelectionContainer {
        id: rightContainer

        visible: root.isStereo

        anchors.top: leftOrMonoContainer.bottom
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom

        canvas: root.canvas
        trackId: root.trackId
        channel: 1
        trackSampleRate: root.sampleRate
        selectionStartPosition: root.selectionStartPosition
        selectionEndPosition: root.selectionEndPosition
        selectionStartFrequency: root.selectionStartFrequency
        selectionEndFrequency: root.selectionEndFrequency
        selectionController: root.selectionController

        onSelectionHorizontalResize: function (x1, x2, completed) {
            root.selectionHorizontalResize(x1, x2, completed)
        }
    }
}
