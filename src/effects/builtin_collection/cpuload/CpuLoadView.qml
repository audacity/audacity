import QtQuick 2.15
import QtQuick.Layouts

import Muse.UiComponents

import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection

BuiltinEffectBase {
    id: root

    property string title: qsTrc("effects/cpuload", "CPU load")
    property bool isApplyAllowed: true

    implicitWidth: column.implicitWidth
    implicitHeight: column.implicitHeight

    builtinEffectModel: CpuLoadViewModelFactory.createModel(root, root.instanceId)
    numNavigationPanels: 1
    property alias cpuLoad: root.builtinEffectModel

    property NavigationPanel knobsNavigationPanel: NavigationPanel {
        name: "CpuLoadKnobs"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }

    QtObject {
        id: prv

        readonly property int innerKnobSpacing: 48
        readonly property int rowSpacing: 16
        readonly property int verticalPadding: 24
        readonly property int horizontalPadding: 42
        readonly property int knobRadius: 24
        readonly property var keys: ["Load", "Spike", "SpikeInterval"]
    }

    ColumnLayout {
        id: column

        width: parent.width
        spacing: prv.rowSpacing

        RoundedRectangle {
            Layout.preferredWidth: knobs.implicitWidth + 2 * prv.horizontalPadding
            Layout.preferredHeight: knobs.implicitHeight + 2 * prv.verticalPadding

            color: ui.theme.backgroundSecondaryColor

            border.color: ui.theme.strokeColor
            border.width: 1

            radius: 4

            RowLayout {
                id: knobs

                anchors.centerIn: parent
                spacing: prv.innerKnobSpacing

                Repeater {
                    model: prv.keys

                    BigParameterKnob {
                        required property string modelData
                        required property int index

                        Layout.fillWidth: true

                        navigation.panel: root.knobsNavigationPanel
                        navigation.order: index

                        parameter: cpuLoad.paramsList[modelData]
                        radius: prv.knobRadius

                        onNewValueRequested: function (key, newValue) {
                            cpuLoad.setParam(key, newValue)
                        }

                        onCommitRequested: cpuLoad.commitSettings()
                    }
                }
            }
        }

        StyledTextLabel {
            Layout.fillWidth: true
            Layout.preferredWidth: knobs.implicitWidth + 2 * prv.horizontalPadding
            Layout.bottomMargin: root.usedDestructively ? prv.rowSpacing : 0

            horizontalAlignment: Text.AlignLeft
            wrapMode: Text.WordWrap
            text: qsTrc("effects/cpuload", "Passes the audio through. Load is the share of one CPU core this effect uses while playing; a spike adds that much CPU time at once, every interval.")
        }
    }
}
