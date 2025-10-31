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

    builtinEffectModel: {
        var model = CompressorViewModelFactory.createModel(root, root.instanceId)
        model.onCompressionCurveChanged.connect(compressionCurve.requestPaint)
        return model
    }
    property alias compressor: root.builtinEffectModel

    Column {
        id: rootColumn

        Component {
            id: dynamicsPanel

            DynamicsPanel {
                width: root.width

                instanceId: compressor.instanceId
                playState: root.playState

                showInputDbModel: CompressorSettingModelFactory.createModel(root, root.instanceId, "showInput")
                showOutputDbModel: CompressorSettingModelFactory.createModel(root, root.instanceId, "showOutput")
                showCompressionDbModel: CompressorSettingModelFactory.createModel(root, root.instanceId, "showActual")
            }
        }

        Loader {
            id: dynamicsPanelLoader

            Component.onCompleted: {
                if (!root.usedDestructively) {
                    sourceComponent = dynamicsPanel
                }
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
                            model: CompressorSettingModelFactory.createModel(root, root.instanceId, modelData)
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
                            model: {
                                var model = CompressorSettingModelFactory.createModel(root, root.instanceId, modelData)
                                model.onValueChanged.connect(function () {
                                    compressionCurve.requestPaint()
                                })
                                return model
                            }
                        }
                    }
                }

                CompressionCurve {
                    id: compressionCurve

                    anchors.bottom: leftGrid.bottom
                    availableHeight: leftGrid.height

                    model: compressor
                }
            }
        }
    }
}
