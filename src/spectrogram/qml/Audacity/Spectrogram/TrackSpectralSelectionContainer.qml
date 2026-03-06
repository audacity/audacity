import QtQuick
import Muse.UiComponents
import Audacity.Spectrogram

Item {
    id: root

    // In
    required property var canvas
    required property int trackId
    required property real sampleRate
    required property real selectionStartPosition
    required property real selectionEndPosition
    required property real selectionStartFrequency
    required property real selectionEndFrequency
    required property real selectionStartTime
    required property real selectionEndTime
    required property real channelHeightRatio
    required property bool isStereo
    required property var selectionController

    // Out
    property bool verticalDragActive: leftOrMonoContainer.verticalDragActive || rightContainer.verticalDragActive

    signal selectionHorizontalResize(real x1, real x2, bool completed)

    Component.onCompleted: {
        contextMenuModel.init()
    }

    TrackSpectrogramContextMenuModel {
        id: contextMenuModel
        trackId: root.trackId
    }

    ContextMenuLoader {
        id: contextMenuLoader

        onHandleMenuItem: function (itemId) {
            contextMenuModel.handleMenuItem(itemId)
        }
    }

    MouseArea {
        id: contextMenuMouseArea
        anchors.fill: parent
        visible: selectionStartFrequency < selectionEndFrequency
        hoverEnabled: true
        acceptedButtons: Qt.RightButton
        onClicked: function (mouse) {
            contextMenuLoader.show(Qt.point(mouse.x, mouse.y), contextMenuModel.items)
        }
    }

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
        selectionStartTime: root.selectionStartTime
        selectionEndTime: root.selectionEndTime
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
        selectionStartTime: root.selectionStartTime
        selectionEndTime: root.selectionEndTime
        selectionController: root.selectionController

        onSelectionHorizontalResize: function (x1, x2, completed) {
            root.selectionHorizontalResize(x1, x2, completed)
        }
    }
}
