import QtQuick 2.15
import Audacity.UiComponents
import Audacity.BuiltinEffectsCollection

GridPlot {
    id: root

    property alias model: painter.model

    function requestPaint() {
        painter.requestPaint()
    }

    QtObject {
        id: prv

        readonly property int min: -36
        readonly property int max: 0
        readonly property int step: 6
        readonly property var ticks: (function () {
                const result = []
                const span = prv.max - prv.min
                for (let i = prv.min; i <= prv.max; i += prv.step) {
                    result.push({
                        label: String(i),
                        position: (i - prv.min) / span
                    })
                }
                return result
            })()
    }

    xTicks: prv.ticks
    yTicks: prv.ticks

    CompressionCurvePainter {
        id: painter
        anchors.fill: parent
        min: -36
        max: 0
    }
}
