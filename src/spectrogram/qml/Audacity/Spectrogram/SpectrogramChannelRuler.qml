import QtQuick

import Muse.UiComponents
import Audacity.Spectrogram

Item {
    id: root

    clip: true

    required property int trackId
    required property real cursorYPos

    SpectrogramChannelRulerModel {
        id: rulerModel
        trackId: root.trackId
        labelHeight: fontMetrics.height
        channelHeight: root.height
    }

    QtObject {
        id: prv
        readonly property int majorTickWidth: 8
        readonly property int minorTickWidth: 4
    }

    FontMetrics {
        id: fontMetrics
        font.family: ui.theme.bodyFont.family
        font.pixelSize: ui.theme.bodyFont.pixelSize
    }

    Row {
        anchors.fill: parent

        Item {
            id: tickColumn
            width: prv.majorTickWidth + 8
            height: parent.height

            Repeater {
                model: rulerModel.majorTicks
                delegate: Rectangle {
                    y: modelData.y - height / 2
                    width: prv.majorTickWidth
                    height: 1
                    color: ui.theme.fontSecondaryColor
                    antialiasing: true
                }
            }

            Repeater {
                model: rulerModel.minorTicks
                delegate: Rectangle {
                    y: modelData.y - height / 2
                    width: prv.minorTickWidth
                    height: 1
                    color: ui.theme.extra["waveform_ruler_small_step_color"]
                    antialiasing: true
                }
            }
        }

        Item {
            id: labelColumn
            width: parent.width - tickColumn.width
            height: parent.height

            Repeater {
                model: rulerModel.majorTicks
                delegate: StyledTextLabel {
                    width: parent.width
                    height: rulerModel.labelHeight
                    y: {
                        // Make sure it doesn't collide with the top or the bottom of the channel.
                        const halfLabelHeight = height / 2
                        return Math.min(Math.max(modelData.y - halfLabelHeight, 0), parent.height - height)
                    }

                    horizontalAlignment: Text.AlignLeft
                    verticalAlignment: Text.AlignVCenter
                    text: modelData.label
                    elide: Text.ElideNone
                    color: ui.theme.extra["waveform_ruler_label_color"]
                }
            }
        }
    }

    Rectangle {
        anchors.left: parent.left
        anchors.right: parent.right
        height: 1
        color: ui.theme.fontPrimaryColor
        y: rulerModel.rulerGuideYPos - height / 2
    }

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.NoButton

        onWheel: function (wheelEvent) {
            if (wheelEvent.modifiers & Qt.ControlModifier) {
                if (wheelEvent.angleDelta.y > 0) {
                    rulerModel.zoomIn(wheelEvent.y)
                } else if (wheelEvent.angleDelta.y < 0) {
                    rulerModel.zoomOut(wheelEvent.y)
                }
            } else {
                // Scroll vertically to scroll the channel.
                rulerModel.scrollBy(wheelEvent.angleDelta.y)
            }
            wheelEvent.accepted = true
        }
    }
}
