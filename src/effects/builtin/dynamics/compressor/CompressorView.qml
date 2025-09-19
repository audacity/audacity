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

    Column {
        id: rootColumn

        DynamicsPanel {
            width: root.width
            visible: !root.usedDestructively

            instanceId: compressor.instanceId
            playState: root.playState
            gridColor: root.gridColor

            showInputDbModel: CompressorSettingModel {
                paramId: "showInput"
            }
            showOutputDbModel: CompressorSettingModel {
                paramId: "showOutput"
            }
            showCompressionDbModel: CompressorSettingModel {
                paramId: "showActual"
            }
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
                        model: ["attackMs", "releaseMs", "lookaheadMs"]

                        delegate: SettingKnob {
                            required property string modelData
                            isVertical: true
                            knobFirst: false
                            warp: true
                            model: CompressorSettingModel {
                                paramId: modelData
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
                        model: ["thresholdDb", "compressionRatio", "kneeWidthDb", "makeupGainDb"]

                        delegate: SettingKnob {
                            required property string modelData
                            isVertical: true
                            knobFirst: false
                            warp: true
                            model: CompressorSettingModel {
                                paramId: modelData
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
