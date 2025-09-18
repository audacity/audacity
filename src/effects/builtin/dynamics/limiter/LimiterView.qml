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

    Component.onCompleted: {
        limiter.init()
    }

    Column {
        id: rootColumn

        DynamicsPanel {
            width: root.width
            visible: !root.usedDestructively

            instanceId: limiter.instanceId
            playState: root.playState
            gridColor: root.gridColor

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

                spacing: 16
                padding: 16

                Column {
                    id: thresholdColumn

                    topPadding: 12
                    bottomPadding: 12

                    SettingKnob {
                        id: thresholdKnob

                        isVertical: true
                        radius: 24
                        title: qsTrc("effects/limiter", "Threshold")
                        unit: "dB"
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
                        title: qsTrc("effects/limiter", "Make-up gain")
                        unit: "dB"
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
                    model: [
                        {
                            id: "lookaheadMs",
                            title: qsTrc("effects/limiter", "Lookahead"),
                            unit: "ms"
                        },
                        {
                            id: "kneeWidthDb",
                            title: qsTrc("effects/limiter", "Knee width"),
                            unit: "dB"
                        },
                        {
                            id: "releaseMs",
                            title: qsTrc("effects/limiter", "Release"),
                            unit: "ms"
                        },
                    ]

                    delegate: SettingKnob {
                        anchors.verticalCenter: parent.verticalCenter
                        required property var modelData
                        isVertical: true
                        knobFirst: false
                        radius: 16
                        title: modelData.title
                        unit: modelData.unit
                        warp: true
                        model: LimiterSettingModel {
                            paramId: modelData.id
                        }
                    }
                }
            }
        }
    }
}
