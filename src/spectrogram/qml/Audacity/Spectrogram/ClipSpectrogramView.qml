import QtQuick 2.15
import QtQuick.Layouts 1.15

import Audacity.Spectrogram 1.0

Item {
    id: root

    required property bool isStereo
    required property double channelHeightRatio
    required property int clipId
    required property int trackId
    required property int timelineIndentWidth
    required property double zoom
    required property double frameStartTime
    required property double frameEndTime
    required property double selectionStartTime
    required property double selectionEndTime

    signal spectrogramMouseClick(int channel, real x, real y, real height)
    signal spectrogramMouseDrag(int channel, real x, real y, real height)
    signal spectrogramMouseRelease(int channel, real x, real y, real height)

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

                onMousePressed: function (point) {
                    root.spectrogramMouseClick(index, point.x, point.y, height)
                }
                onMouseDragged: function (point) {
                    root.spectrogramMouseDrag(index, point.x, point.y, height)
                }
                onMouseReleased: function (point) {
                    root.spectrogramMouseRelease(index, point.x, point.y, height)
                }
            }
        }
    }
}
