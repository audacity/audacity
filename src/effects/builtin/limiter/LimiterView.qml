import QtQuick
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects
import Audacity.ProjectScene
import Audacity.BuiltinEffects

import "../common"

BuiltinEffectBase {
    id: root

    property string title: qsTrc("effects/limiter", "Limiter")
    property bool isApplyAllowed: true

    width: knobRow.width
    implicitHeight: knobRow.height

    model: limiter

    LimiterViewModel {
        id: limiter
    }

    Component.onCompleted: {
        limiter.init()
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
                        unit: "ms",
                        warpingType: ValueWarpingType.Aggressive
                    },
                    {
                        id: "kneeWidthDb",
                        title: qsTrc("effects/limiter", "Knee width"),
                        unit: "dB",
                        warpingType: ValueWarpingType.None
                    },
                    {
                        id: "releaseMs",
                        title: qsTrc("effects/limiter", "Release"),
                        unit: "ms",
                        warpingType: ValueWarpingType.Soft
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
                    warpingType: modelData.warpingType
                    model: LimiterSettingModel {
                        paramId: modelData.id
                    }
                }
            }
        }
    }
}
