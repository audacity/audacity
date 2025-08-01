import QtQuick
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects

import "../common"

EffectBase {
    id: root

    property string title: qsTrc("effects/compressor", "Compressor")
    property bool isApplyAllowed: true

    width: row.width
    implicitHeight: row.height

    model: compressor

    CompressorViewModel {
        id: compressor

        instanceId: root.instanceId
        onCompressionCurveChanged: {
            compressionCurve.requestPaint()
        }
    }

    Component.onCompleted: {
        compressor.init()
    }

    Rectangle {
        width: row.width
        height: row.height

        radius: 4

        color: ui.theme.backgroundSecondaryColor
        border.color: ui.theme.strokeColor

        Row {
            id: row

            padding: 16
            spacing: 24

            Grid {
                id: leftGrid

                columns: 2
                spacing: 24

                Repeater {
                    model: [
                        { id: "attackMs", title: qsTrc("effects/compressor", "Attack"), unit: "ms" },
                        { id: "releaseMs", title: qsTrc("effects/compressor", "Release"), unit: "ms" },
                        { id: "lookaheadMs", title: qsTrc("effects/compressor", "Lookahead"), unit: "ms" },
                    ]

                    delegate: SettingKnob {
                        required property var modelData
                        isVertical: true
                        title: modelData.title
                        unit: modelData.unit
                        model: CompressorSettingModel {
                            paramId: modelData.id
                            instanceId: root.instanceId
                        }
                    }
                }
            }

            // Can't use a SeparatorLine in a Row or Column, or we get an infinite loop.
            Rectangle {
                width: 1
                height: leftGrid.height
                color: ui.theme.strokeColor
            }

            Grid {
                columns: 2
                spacing: 24

                Repeater {
                    model: [
                        { id: "thresholdDb", title: qsTrc("effects/compressor", "Threshold"), unit: "dB" },
                        { id: "compressionRatio", title: qsTrc("effects/compressor", "Ratio"), unit: "" },
                        { id: "kneeWidthDb", title: qsTrc("effects/compressor", "Knee width"), unit: "dB" },
                        { id: "makeupGainDb", title: qsTrc("effects/compressor", "Make-up gain"), unit: "dB" }
                    ]

                    delegate: SettingKnob {
                        required property var modelData
                        isVertical: true
                        title: modelData.title
                        unit: modelData.unit
                        model: CompressorSettingModel {
                            paramId: modelData.id
                            instanceId: root.instanceId
                            onValueChanged: {
                                compressionCurve.requestPaint()
                            }
                        }
                    }
                }
            }

            CompressionCurve {
                id: compressionCurve

                model: compressor
                anchors.bottom: leftGrid.bottom
                availableHeight: leftGrid.height
            }
        }
    }
}
