/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Controls
import Muse.UiComponents
import Audacity.BuiltinEffects

import "./meters"

Item {
    id: root

    required property int instanceId

    property alias playState: stopwatch.playState
    onPlayStateChanged: {
        if (playState === Stopwatch.Playing && prv.prevPlayState === Stopwatch.Stopped) {
            timeline.clear()
        }
        prv.prevPlayState = playState
    }

    height: rootColumn.height

    QtObject {
        id: prv

        readonly property int timelineHeight: 280

        readonly property int meterWidth: 11
        readonly property int meterSpacing: 4

        readonly property int dbMin: -48
        readonly property int duration: 3
        readonly property int dbStep: 6

        function dbToY(db) {
            return db / dbMin * timelineHeight
        }

        readonly property var dbSteps: {
            let arr = []
            for (let v = 0; v >= dbMin; v -= dbStep) {
                arr.push(v)
            }
            return arr
        }
        readonly property var timeSteps: {
            const step = 1
            let arr = []
            for (let t = -duration; t <= 0; t += step) {
                arr.push(t)
            }
            return arr
        }

        property int prevPlayState: playState
    }

    Component.onCompleted: {
        timelineSourceModel.init()
        compressionDbMeterModel.init()
        outputDbMeterModel.init()
    }

    Stopwatch {
        id: stopwatch

        onTick: {
            compressionDbMeterModel.update()
            outputDbMeterModel.update()
        }
    }

    TimelineSourceModel {
        id: timelineSourceModel

        instanceId: root.instanceId
        onNewSamples: function (samples) {
            timeline.onNewSamples(samples)
        }
    }

    Column {
        id: rootColumn

        padding: 0
        spacing: 0

        Row {
            id: checkboxRow

            height: 40

            spacing: 16

            StyledTextLabel {
                text: qsTrc("effects/compressor", "Show:")
                anchors.verticalCenter: parent.verticalCenter

                font.bold: true
            }

            Repeater {
                model: [
                    {
                        text: qsTrc("effects/compressor", "Input"),
                        property: "showInputDb"
                    },
                    {
                        text: qsTrc("effects/compressor", "Output"),
                        property: "showOutputDb"
                    },
                    {
                        text: qsTrc("effects/compressor", "Compression"),
                        property: "showCompressionDb"
                    }
                ]

                delegate: CheckBox {
                    anchors.verticalCenter: parent.verticalCenter

                    text: modelData.text
                    checked: timeline[modelData.property]
                    onClicked: timeline[modelData.property] = !timeline[modelData.property]
                }
            }
        }

        Row {
            id: timelineRow

            spacing: 6

            Rectangle {
                id: timelineArea

                // Not sure why, but adding this effect causes the UI not to update properly.
                // layer.enabled: true
                // layer.effect: RoundedCornersEffect {
                //     radius: 3
                // }

                color: DynamicsColors.backgroundColor
                border.color: ui.theme.strokeColor
                border.width: 1
                width: root.width - meterGrid.width - 24 // TODO font metrics
                height: prv.timelineHeight
                clip: true

                Repeater {
                    id: horizontalGridLines

                    model: prv.dbSteps

                    delegate: Rectangle {
                        width: timeline.width
                        height: 1
                        y: modelData / prv.dbMin * prv.timelineHeight
                        color: ui.theme.strokeColor
                    }
                }

                Repeater {
                    id: verticalGridLines

                    model: prv.timeSteps

                    delegate: Rectangle {
                        width: 1
                        height: timeline.height
                        x: (modelData + prv.duration) / prv.duration * timeline.width
                        color: ui.theme.strokeColor
                    }
                }

                DynamicsTimeline {
                    id: timeline

                    anchors.fill: parent
                    stopwatchTime: stopwatch.elapsedTime

                    dbMin: prv.dbMin
                    duration: prv.duration
                    dataPointRate: timelineSourceModel.dataPointRate
                }
            }

            Column {
                id: meterGrid

                y: -clipIndicator.height - spacing

                spacing: 2

                Row {
                    id: clipIndicatorRow

                    spacing: prv.meterSpacing
                    leftPadding: prv.meterSpacing
                    rightPadding: prv.meterSpacing

                    Item {
                        width: prv.meterWidth
                        height: 1
                    }

                    ClipIndicator {
                        id: clipIndicator

                        width: prv.meterWidth
                        height: 10

                        isClipping: outputDbMeterModel.isClipping
                        onClicked: {
                            outputDbMeterModel.isClipping = false
                            outputDbMeterModel.reset()
                        }
                    }
                }

                Row {
                    id: meterRow

                    height: prv.timelineHeight

                    spacing: prv.meterSpacing
                    leftPadding: prv.meterSpacing
                    rightPadding: prv.meterSpacing

                    DynamicsMeter {
                        id: compressionMeter

                        CompressionDbMeterModel {
                            id: compressionDbMeterModel
                            instanceId: root.instanceId
                            playState: root.playState
                        }

                        width: prv.meterWidth
                        height: parent.height

                        currentMax: compressionDbMeterModel.currentMax
                        globalMax: compressionDbMeterModel.globalMax

                        upwards: false
                        dbMin: prv.dbMin
                        areaColor: DynamicsColors.timelineCompressionDbColor
                        onClicked: {
                            compressionDbMeterModel.reset()
                        }
                    }

                    DynamicsMeter {
                        id: outputMeter

                        OutputDbMeterModel {
                            id: outputDbMeterModel
                            instanceId: root.instanceId
                            playState: root.playState
                        }

                        width: prv.meterWidth
                        height: parent.height

                        currentMax: outputDbMeterModel.currentMax
                        globalMax: outputDbMeterModel.globalMax

                        upwards: true
                        dbMin: prv.dbMin
                        areaColor: DynamicsColors.timelineDataFillColor
                        onClicked: {
                            outputDbMeterModel.reset()
                            outputDbMeterModel.isClipping = false
                        }
                    }
                }
            }

            Item {
                id: dbLabelsContainer

                width: 1
                height: 1

                Repeater {
                    model: prv.dbSteps

                    Text {
                        y: timeline.y + prv.dbToY(modelData) - 8 // todo font metrics trick

                        text: modelData
                        font.pixelSize: 12
                        color: "black"
                    }
                }
            }
        }
    }
}
