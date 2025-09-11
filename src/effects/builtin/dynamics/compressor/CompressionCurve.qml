import QtQuick 2.15
import Muse.Ui
import Muse.UiComponents
import Audacity.Effects

Item {
    id: root

    property alias model: painter.model
    required property int availableHeight

    function requestPaint() {
        painter.requestPaint()
    }

    width: prv.labelWidth + prv.labelMargin + background.width + prv.extraLabelSpace
    height: prv.labelHeight + prv.labelMargin + background.height

    QtObject {
        id: prv
        readonly property int min: -60
        readonly property int max: 0
        readonly property int step: 12
        readonly property int tickLength: 4
        readonly property var ticks: (function () {
                var result = []
                for (var i = prv.min; i <= prv.max; i += prv.step) {
                    result.push(i)
                }
                return result
            })()
        readonly property color lineColor: "#7B7B81"

        property int labelWidth: fontMetrics.boundingRect("-000").width
        property int labelHeight: fontMetrics.boundingRect("0").height
        readonly property int labelMargin: 4
        readonly property int extraLabelSpace: 8
    }

    FontMetrics {
        id: fontMetrics
        font.family: ui.theme.bodyFont.family
        font.pixelSize: ui.theme.bodyFont.pixelSize
    }

    Rectangle {
        id: background

        y: prv.labelHeight + prv.labelMargin
        width: 312
        height: availableHeight - prv.labelHeight - prv.labelMargin - prv.extraLabelSpace
        color: "#313147"

        Repeater {
            id: verticalLines

            model: prv.ticks
            delegate: Item {
                x: background.width * index / (prv.ticks.length - 1)
                y: -prv.tickLength

                StyledTextLabel {
                    width: prv.labelWidth
                    height: prv.labelHeight
                    horizontalAlignment: Text.AlignHCenter
                    anchors.horizontalCenter: vLine.horizontalCenter
                    anchors.bottom: vLine.top
                    anchors.bottomMargin: prv.labelMargin
                    text: modelData
                }

                Rectangle {
                    id: vLine

                    width: 1
                    height: background.height + prv.tickLength
                    color: prv.lineColor
                }
            }
        }

        Repeater {
            id: horizontalLines
            model: prv.ticks
            delegate: Item {
                y: background.height * (1 - index / (prv.ticks.length - 1))

                StyledTextLabel {
                    width: prv.labelWidth
                    height: prv.labelHeight
                    horizontalAlignment: Text.AlignRight
                    anchors.left: hLine.right
                    anchors.leftMargin: prv.labelMargin
                    y: hLine.y - (fontMetrics.ascent + fontMetrics.descent) / 2
                    text: modelData
                }

                Rectangle {
                    id: hLine

                    width: background.width + prv.tickLength
                    height: 1
                    color: prv.lineColor
                }
            }
        }

        CompressionCurvePainter {
            id: painter
            anchors.fill: parent
            min: prv.min
            max: prv.max
        }
    }
}
