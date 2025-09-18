/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects
import Audacity.BuiltinEffects

import "../timeline"

DynamicsEffectBase {
    id: root

    property string title: qsTrc("effects/compressor", "Compressor")
    property bool isApplyAllowed: true

    width: rootColumn.width
    implicitHeight: rootColumn.height

    model: compressor

    CompressorViewModel {
        id: compressor

        onCompressionCurveChanged: {
            compressionCurve.requestPaint()
        }
    }

    Component.onCompleted: {
        compressor.init()
    }

    Column {
        id: rootColumn

        DynamicsPanel {
            width: root.width
            visible: !root.usedDestructively

            instanceId: compressor.instanceId
            playState: root.playState
            gridColor: root.gridColor
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
                            {
                                id: "attackMs",
                                title: qsTrc("effects/compressor", "Attack"),
                                unit: "ms"
                            },
                            {
                                id: "releaseMs",
                                title: qsTrc("effects/compressor", "Release"),
                                unit: "ms"
                            },
                            {
                                id: "lookaheadMs",
                                title: qsTrc("effects/compressor", "Lookahead"),
                                unit: "ms"
                            },
                        ]

                        delegate: SettingKnob {
                            required property var modelData
                            isVertical: true
                            knobFirst: false
                            title: modelData.title
                            unit: modelData.unit
                            warp: true
                            model: CompressorSettingModel {
                                paramId: modelData.id
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
                            {
                                id: "thresholdDb",
                                title: qsTrc("effects/compressor", "Threshold"),
                                unit: "dB"
                            },
                            {
                                id: "compressionRatio",
                                title: qsTrc("effects/compressor", "Ratio"),
                                unit: ""
                            },
                            {
                                id: "kneeWidthDb",
                                title: qsTrc("effects/compressor", "Knee width"),
                                unit: "dB"
                            },
                            {
                                id: "makeupGainDb",
                                title: qsTrc("effects/compressor", "Make-up gain"),
                                unit: "dB"
                            }
                        ]

                        delegate: SettingKnob {
                            required property var modelData
                            isVertical: true
                            knobFirst: false
                            title: modelData.title
                            unit: modelData.unit
                            warp: true
                            model: CompressorSettingModel {
                                paramId: modelData.id
                                onValueChanged: {
                                    compressionCurve.requestPaint()
                                }
                            }
                        }
                    }
                }

                CompressionCurve {
                    id: compressionCurve

                    anchors.bottom: leftGrid.bottom
                    availableHeight: leftGrid.height

                    model: compressor
                    gridColor: root.gridColor
                }
            }
        }
    }
}
