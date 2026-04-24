import QtQuick
import Muse.UiComponents
import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection

BuiltinEffectBase {
    id: root

    property string title: qsTrc("effects/slidingstretch", "Sliding stretch")
    property bool isApplyAllowed: true

    width: 528
    implicitHeight: mainGrid.height

    builtinEffectModel: SlidingStretchViewModelFactory.createModel(root, root.instanceId)
    numNavigationPanels: 4
    property alias slidingStretch: root.builtinEffectModel

    QtObject {
        id: prv

        readonly property int cardWidth: (mainGrid.width - mainGrid.columnSpacing) / 2
        readonly property int shortCardHeight: 92
        readonly property int tallCardHeight: 156
    }

    Grid {
        id: mainGrid

        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right

        columns: 2
        columnSpacing: 16
        rowSpacing: 16

        SlidingStretchCard {
            width: prv.cardWidth
            height: prv.shortCardHeight
            mode: "tempo"
            tempoParam: SlidingStretchSettingModelFactory.createModel(root, root.instanceId, "initialTempo")
            viewModel: slidingStretch
            title: qsTrc("effects/slidingstretch", "Initial tempo change")

            navPanel.section: root.dialogView.navigationSection
            navPanel.order: 0
            navPanel.name: "SlidingStretchInitialTempo"
        }

        SlidingStretchCard {
            width: prv.cardWidth
            height: prv.shortCardHeight
            mode: "tempo"
            tempoParam: SlidingStretchSettingModelFactory.createModel(root, root.instanceId, "finalTempo")
            viewModel: slidingStretch
            title: qsTrc("effects/slidingstretch", "Final tempo change")

            navPanel.section: root.dialogView.navigationSection
            navPanel.order: 1
            navPanel.name: "SlidingStretchFinalTempo"
        }

        SlidingStretchCard {
            width: prv.cardWidth
            height: prv.tallCardHeight
            mode: "pitch"
            pitchSemitonesParam: SlidingStretchSettingModelFactory.createModel(root, root.instanceId, "initialPitchSemitones")
            pitchPctParam: SlidingStretchSettingModelFactory.createModel(root, root.instanceId, "initialPitchPct")
            viewModel: slidingStretch
            title: qsTrc("effects/slidingstretch", "Initial pitch shift")

            navPanel.section: root.dialogView.navigationSection
            navPanel.order: 2
            navPanel.name: "SlidingStretchInitialPitch"
        }

        SlidingStretchCard {
            width: prv.cardWidth
            height: prv.tallCardHeight
            mode: "pitch"
            pitchSemitonesParam: SlidingStretchSettingModelFactory.createModel(root, root.instanceId, "finalPitchSemitones")
            pitchPctParam: SlidingStretchSettingModelFactory.createModel(root, root.instanceId, "finalPitchPct")
            viewModel: slidingStretch
            title: qsTrc("effects/slidingstretch", "Final pitch shift")

            navPanel.section: root.dialogView.navigationSection
            navPanel.order: 3
            navPanel.name: "SlidingStretchFinalPitch"
        }
    }
}
