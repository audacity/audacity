import QtQuick
import QtQuick.Controls
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

        readonly property color gridColor: "#666666"
        readonly property int timelineHeight: 280

        readonly property int meterWidth: 11
        readonly property int meterSpacing: 4

        readonly property int dbMin: -20
        readonly property int duration: 5
        readonly property int dbStep: 3

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

            Text {
                text: "Show:"
                anchors.verticalCenter: parent.verticalCenter

                font.bold: true
            }

            Repeater {
                model: [
                    {
                        text: "Input",
                        property: "showInputDb"
                    },
                    {
                        text: "Output",
                        property: "showOutputDb"
                    },
                    {
                        text: "Compression",
                        property: "showCompressionDb"
                    }
                ]

                delegate: CheckBox {
                    anchors.verticalCenter: parent.verticalCenter

                    spacing: 16
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

                color: "#212433"
                width: root.width - meterGrid.width - 24 // TODO font metrics
                height: prv.timelineHeight
                radius: 10
                clip: true

                Repeater {
                    id: horizontalGridLines

                    model: prv.dbSteps

                    delegate: Rectangle {
                        width: timeline.width
                        height: 1
                        y: modelData / prv.dbMin * prv.timelineHeight
                        color: prv.gridColor
                    }
                }

                Repeater {
                    id: verticalGridLines

                    model: prv.timeSteps

                    delegate: Rectangle {
                        width: 1
                        height: timeline.height
                        x: (modelData + prv.duration) / prv.duration * timeline.width
                        color: prv.gridColor
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

                Rectangle {
                    id: awaitingPlaybackMessageBackground

                    anchors.fill: parent

                    visible: stopwatch.playState === Stopwatch.Stopped
                    opacity: 0.75
                    color: "white"
                    radius: 3

                    Text {
                        anchors.centerIn: parent
                        text: "Awaiting playback"
                        font.pixelSize: 24
                        color: "gray"
                    }
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

                    Rectangle {
                        id: clipIndicator

                        width: prv.meterWidth
                        height: 10
                        radius: 3

                        color: timelineSourceModel.isClipping ? "darkred" : "lightgray"

                        MouseArea {
                            anchors.fill: parent
                            cursorShape: Qt.PointingHandCursor
                            onClicked: timelineSourceModel.isClipping = false
                        }
                    }
                }

                Rectangle {
                    id: meterBackground

                    width: meterRow.width
                    height: timelineArea.height
                    color: timelineArea.color
                    radius: 3

                    Repeater {
                        id: meterBackgroundGridlines

                        model: prv.dbSteps
                        delegate: Rectangle {
                            width: meterBackground.width
                            height: 1
                            y: prv.dbToY(modelData)

                            color: prv.gridColor
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
                            }

                            width: prv.meterWidth
                            height: parent.height

                            currentMax: compressionDbMeterModel.currentMax
                            fiveSecMax: compressionDbMeterModel.fiveSecMax

                            upwards: false
                            dbMin: prv.dbMin
                            areaColor: "#80FFD12C" // TODO sync with timeline
                            lineColor: "#FFD12C" // TODO sync with timeline
                            clipIndicatorHeight: clipIndicator.height
                        }

                        DynamicsMeter {
                            id: outputMeter

                            OutputDbMeterModel {
                                id: outputDbMeterModel
                                instanceId: root.instanceId
                            }

                            width: prv.meterWidth
                            height: parent.height

                            currentMax: outputDbMeterModel.currentMax
                            fiveSecMax: outputDbMeterModel.fiveSecMax

                            upwards: true
                            dbMin: prv.dbMin
                            areaColor: "#80565695" // TODO sync with timeline
                            lineColor: "#80FFFFFF" // TODO sync with timeline
                            clipIndicatorHeight: clipIndicator.height
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
