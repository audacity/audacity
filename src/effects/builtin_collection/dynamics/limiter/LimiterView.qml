/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts
import Muse.Ui 1.0
import Muse.UiComponents
import Audacity.Effects
import Audacity.ProjectScene
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection

import "../timeline"

DynamicsEffectBase {
    id: root

    property string title: qsTrc("effects/limiter", "Limiter")
    property bool isApplyAllowed: true

    width: rootColumn.width
    implicitHeight: rootColumn.height

    builtinEffectModel: LimiterViewModelFactory.createModel(root, root.instanceId)
    property alias limiter: root.builtinEffectModel
    property NavigationPanel thresholdAndMakeupNavigationPanel: NavigationPanel {
        name: "LimiterThresholdAndMakeup"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }
    property NavigationPanel rightKnobsNavigationPanel: NavigationPanel {
        name: "LimiterRightKnobs"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 2
    }

    Column {
        id: rootColumn

        Component {
            id: dynamicsPanel

            DynamicsPanel {
                width: root.width
                visible: !root.usedDestructively

                instanceId: limiter.instanceId
                playState: root.playState

                showInputDbModel: LimiterSettingModelFactory.createModel(root, root.instanceId, "showInput")
                showOutputDbModel: LimiterSettingModelFactory.createModel(root, root.instanceId, "showOutput")
                showCompressionDbModel: LimiterSettingModelFactory.createModel(root, root.instanceId, "showActual")

                // Specific to limiter
                dbMin: -12
                dbStep: 3
                duration: 2
                timelineHeight: knobRow.height
                needsClipIndicator: false // Clipping with the limiter is impossible.
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

                        navigationPanel: root.thresholdAndMakeupNavigationPanel
                        navigationOrder: 0
                        isVertical: true
                        radius: 24
                        warp: true
                        model: LimiterSettingModelFactory.createModel(root, root.instanceId, "thresholdDb")
                    }
                }

                Column {
                    topPadding: 12
                    bottomPadding: 12

                    SettingKnob {
                        id: makeupKnob

                        navigationPanel: root.thresholdAndMakeupNavigationPanel
                        navigationOrder: thresholdKnob.navigationOrder + 1
                        isVertical: true
                        radius: 24
                        warp: true
                        model: LimiterSettingModelFactory.createModel(root, root.instanceId, "makeupTargetDb")
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
                        required property int index
                        navigationPanel: root.rightKnobsNavigationPanel
                        navigationOrder: index
                        anchors.verticalCenter: parent.verticalCenter
                        isVertical: true
                        knobFirst: false
                        radius: 16
                        warp: true
                        model: LimiterSettingModelFactory.createModel(root, root.instanceId, modelData)
                    }
                }
            }
        }
    }
}
