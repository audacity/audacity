import QtQuick
import QtQuick.Layouts

import Muse.UiComponents

import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection

BuiltinEffectBase {
    id: root

    //: Title of the Phaser effect dialog
    property string title: qsTrc("effects/phaser", "Phaser")
    property bool isApplyAllowed: true

    implicitWidth: row.implicitWidth
    implicitHeight: row.implicitHeight

    builtinEffectModel: PhaserViewModelFactory.createModel(root, root.instanceId)
    numNavigationPanels: 2

    property NavigationPanel mainNavigationPanel: NavigationPanel {
        name: "PhaserMain"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }

    property NavigationPanel secondaryNavigationPanel: NavigationPanel {
        name: "PhaserSecondary"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 2
    }

    QtObject {
        id: prv

        readonly property int narrowRowSpacing: 16
        readonly property int rowSpacing: 24
        readonly property int columnSpacing: 32
        readonly property int cardRadius: 4
        readonly property int knobRadius: 24
    }

    RowLayout {
        id: row

        width: parent.width
        spacing: prv.rowSpacing

        RoundedRectangle {
            Layout.preferredWidth: mainGrid.implicitWidth + 2 * prv.rowSpacing
            Layout.preferredHeight: mainGrid.implicitHeight + 2 * prv.columnSpacing

            color: ui.theme.backgroundSecondaryColor

            border.color: ui.theme.strokeColor
            border.width: 1

            radius: prv.cardRadius

            GridLayout {
                id: mainGrid

                anchors.left: parent.left
                anchors.top: parent.top
                anchors.leftMargin: prv.rowSpacing
                anchors.topMargin: prv.columnSpacing

                rows: 2
                columns: 2
                rowSpacing: prv.rowSpacing
                columnSpacing: prv.columnSpacing

                Repeater {
                    model: ["Freq", "Depth", "Stages", "Feedback"]

                    delegate: SettingKnob {
                        required property string modelData
                        required property int index

                        Layout.fillWidth: true

                        navigationPanel: root.mainNavigationPanel
                        navigationOrder: index

                        isVertical: true
                        radius: prv.knobRadius
                        model: PhaserSettingModelFactory.createModel(root, root.instanceId, modelData)
                    }
                }
            }
        }

        ColumnLayout {
            Layout.fillHeight: true
            Layout.fillWidth: true

            spacing: prv.narrowRowSpacing

            SettingKnob {
                Layout.fillWidth: true

                navigationPanel: root.secondaryNavigationPanel
                navigationOrder: 0

                model: PhaserSettingModelFactory.createModel(root, root.instanceId, "Phase")
            }

            SeparatorLine {}

            Repeater {
                model: ["DryWet", "Gain"]

                delegate: SettingKnob {
                    required property string modelData
                    required property int index

                    Layout.fillWidth: true

                    navigationPanel: root.secondaryNavigationPanel
                    navigationOrder: index + 1

                    model: PhaserSettingModelFactory.createModel(root, root.instanceId, modelData)
                }
            }
        }
    }
}
