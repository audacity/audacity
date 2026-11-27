import QtQuick 2.15
import QtQuick.Layouts

import Muse.UiComponents

import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection

BuiltinEffectBase {
    id: root

    property string title: qsTrc("effects/reverb", "Reverb")
    property bool isApplyAllowed: true

    implicitWidth: row.implicitWidth
    implicitHeight: row.implicitHeight

    builtinEffectModel: ReverbViewModelFactory.createModel(root, root.instanceId)
    numNavigationPanels: 2
    property alias reverb: root.builtinEffectModel
    property NavigationPanel leftColumnNavigationPanel: NavigationPanel {
        name: "ReverbLeftColumn"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }
    property NavigationPanel rightColumnNavigationPanel: NavigationPanel {
        name: "ReverbRightColumn"
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
    }

    RowLayout {
        id: row

        width: parent.width
        spacing: prv.rowSpacing

        RoundedRectangle {

            Layout.preferredWidth: leftColumn.implicitWidth + 2 * prv.rowSpacing
            Layout.preferredHeight: rightColumn.height

            color: ui.theme.backgroundSecondaryColor

            border.color: ui.theme.strokeColor
            border.width: 1

            radius: 4

            GridLayout {
                id: leftColumn

                anchors.left: parent.left
                anchors.top: parent.top
                anchors.leftMargin: prv.rowSpacing
                anchors.topMargin: prv.columnSpacing

                rows: 2
                columns: 2
                rowSpacing: prv.rowSpacing
                columnSpacing: prv.columnSpacing

                Repeater {
                    model: ["RoomSize", "StereoWidth", "PreDelay"]

                    delegate: SettingKnob {
                        required property string modelData
                        required property int index

                        Layout.fillWidth: true

                        navigationPanel: root.leftColumnNavigationPanel
                        navigationOrder: index

                        isVertical: true
                        radius: 24
                        model: ReverbSettingModelFactory.createModel(root, root.instanceId, modelData)
                    }
                }
            }
        }

        GridLayout {
            id: rightColumn

            Layout.fillHeight: true
            Layout.fillWidth: true

            columns: 2
            rowSpacing: prv.narrowRowSpacing
            columnSpacing: prv.columnSpacing

            Item {
                Layout.columnSpan: 2
                Layout.preferredHeight: 1
            }

            Repeater {
                model: ["HfDamping", "Reverberance", "ToneLow", "ToneHigh"]

                delegate: SettingKnob {
                    required property string modelData
                    required property int index

                    Layout.fillWidth: true

                    navigationPanel: root.rightColumnNavigationPanel
                    navigationOrder: index

                    model: ReverbSettingModelFactory.createModel(root, root.instanceId, modelData)
                }
            }

            SeparatorLine {
                Layout.columnSpan: 2
            }

            Repeater {
                model: ["WetGain", "DryGain"]

                delegate: SettingKnob {
                    required property string modelData
                    required property int index

                    Layout.fillWidth: true

                    navigationPanel: root.rightColumnNavigationPanel
                    navigationOrder: index + 4

                    model: ReverbSettingModelFactory.createModel(root, root.instanceId, modelData)
                }
            }

            CheckBox {
                id: wetOnly

                navigation.panel: root.rightColumnNavigationPanel
                navigation.order: 6

                text: qsTrc("effects/reverb", "Wet only")
                checked: reverb.wetOnly
                onClicked: function () {
                    reverb.wetOnly = !reverb.wetOnly
                    reverb.commitSettings()
                }
            }

            Item {
                Layout.row: 6
                Layout.preferredHeight: prv.narrowRowSpacing
            }
        }
    }
}
