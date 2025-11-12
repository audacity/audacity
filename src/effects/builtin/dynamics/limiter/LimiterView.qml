/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects
import Audacity.ProjectScene
import Audacity.BuiltinEffects

import "../timeline"

DynamicsEffectBase {
    id: root

    property string title: qsTrc("effects/limiter", "Limiter")
    property bool isApplyAllowed: true

    width: rootColumn.width
    implicitHeight: rootColumn.height

    model: limiter

    LimiterViewModel {
        id: limiter
    }

    Column {
        id: rootColumn

        DynamicsPanel {
            width: root.width
            visible: !root.usedDestructively

            instanceId: limiter.instanceId
            playState: root.playState

            showInputDbModel: LimiterSettingModel {
                paramId: "showInput"
            }
            showOutputDbModel: LimiterSettingModel {
                paramId: "showOutput"
            }
            showCompressionDbModel: LimiterSettingModel {
                paramId: "showActual"
            }

            // Specific to limiter
            dbMin: -12
            dbStep: 3
            duration: 2
            timelineHeight: knobRow.height
            needsClipIndicator: false // Clipping with the limiter is impossible.
        }

        Rectangle {

            color: ui.theme.backgroundSecondaryColor
            border.color: ui.theme.strokeColor
            width: knobRow.width
            height: knobRow.height
            radius: 4

            Row {
                id: knobRow

                spacing: ui.theme.extra.space_16
                padding: 16

                Column {
                    id: thresholdColumn

                    topPadding: 12
                    bottomPadding: 12

                    SettingKnob {
                        id: thresholdKnob

                        isVertical: true
                        radius: 24
                        warp: true
                        model: LimiterSettingModel {
                            paramId: "thresholdDb"
                        }
                    }
                }

                Column {
                    topPadding: 12
                    bottomPadding: 12

                    SettingKnob {
                        id: makeupKnob

                        isVertical: true
                        radius: 24
                        warp: true
                        model: LimiterSettingModel {
                            paramId: "makeupTargetDb"
                        }
                    }
                }

                // Can't use a SeparatorLine in a Row or Column, or we get an infinite loop.
                Rectangle {
                    width: 1
                    height: thresholdColumn.height
                    color: ui.theme.strokeColor
                }

                Repeater {
                    model: ["lookaheadMs", "kneeWidthDb", "releaseMs"]

                    delegate: SettingKnob {
                        required property string modelData
                        anchors.verticalCenter: parent.verticalCenter
                        isVertical: true
                        knobFirst: false
                        radius: 16
                        warp: true
                        model: LimiterSettingModel {
                            paramId: modelData
                        }
                    }
                }
            }
        }
    }
}
