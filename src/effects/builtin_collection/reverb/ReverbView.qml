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

    implicitHeight: row.height
    width: 560

    builtinEffectModel: ReverbViewModelFactory.createModel(root, root.instanceId)
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

    function newParameterValueRequested(key, value) {
        reverb.setParam(key, value)
    }

    RowLayout {
        id: row

        width: parent.width
        spacing: prv.rowSpacing

        RoundedRectangle {

            Layout.fillWidth: true
            Layout.preferredWidth: 5
            Layout.preferredHeight: rightColumn.height

            color: ui.theme.backgroundSecondaryColor

            border.color: ui.theme.strokeColor
            border.width: 1

            radius: 4

            GridLayout {
                id: leftColumn

                x: prv.rowSpacing
                y: prv.columnSpacing

                Layout.fillWidth: true

                rows: 2
                columns: 2
                rowSpacing: prv.rowSpacing
                columnSpacing: prv.columnSpacing

                BigParameterKnob {
                    id: roomSizeKnob

                    Layout.fillWidth: true

                    navigation.panel: root.leftColumnNavigationPanel
                    navigation.order: 0

                    parameter: reverb.paramsList["RoomSize"]
                    radius: 24

                    onNewValueRequested: function (key, newValue) {
                        newParameterValueRequested(key, newValue)
                    }

                    onCommitRequested: reverb.commitSettings()
                }

                BigParameterKnob {
                    id: stereoWidthKnob

                    Layout.fillWidth: true

                    navigation.panel: root.leftColumnNavigationPanel
                    navigation.order: roomSizeKnob.navigation.order + 1

                    parameter: reverb.paramsList["StereoWidth"]
                    radius: 24

                    onNewValueRequested: function (key, newValue) {
                        newParameterValueRequested(key, newValue)
                    }

                    onCommitRequested: reverb.commitSettings()
                }

                BigParameterKnob {
                    id: preDelayKnob

                    Layout.fillWidth: true

                    navigation.panel: root.leftColumnNavigationPanel
                    navigation.order: stereoWidthKnob.navigation.order + 1

                    parameter: reverb.paramsList["PreDelay"]
                    radius: 24

                    onNewValueRequested: function (key, newValue) {
                        newParameterValueRequested(key, newValue)
                    }

                    onCommitRequested: reverb.commitSettings()
                }
            }
        }

        GridLayout {
            id: rightColumn

            Layout.fillHeight: true
            Layout.fillWidth: true
            Layout.preferredWidth: 7

            columns: 2
            rows: 5
            rowSpacing: prv.narrowRowSpacing
            columnSpacing: prv.columnSpacing

            Item {
                Layout.columnSpan: 2
                Layout.preferredHeight: 1
            }

            ParameterKnob {
                id: hfDampingKnob

                Layout.fillWidth: true

                navigation.panel: root.rightColumnNavigationPanel
                navigation.order: 0

                parameter: reverb.paramsList["HfDamping"]

                onNewValueRequested: function (key, newValue) {
                    newParameterValueRequested(key, newValue)
                    hfDampingKnob.value = newValue
                }

                onCommitRequested: reverb.commitSettings()
            }

            ParameterKnob {
                id: reverberanceKnob

                Layout.fillWidth: true

                navigation.panel: root.rightColumnNavigationPanel
                navigation.order: hfDampingKnob.navigation.order + 1

                parameter: reverb.paramsList["Reverberance"]

                onNewValueRequested: function (key, newValue) {
                    newParameterValueRequested(key, newValue)
                    reverberanceKnob.value = newValue
                }

                onCommitRequested: reverb.commitSettings()
            }

            ParameterKnob {
                id: toneLowKnob

                Layout.fillWidth: true

                navigation.panel: root.rightColumnNavigationPanel
                navigation.order: reverberanceKnob.navigation.order + 1

                parameter: reverb.paramsList["ToneLow"]

                onNewValueRequested: function (key, newValue) {
                    newParameterValueRequested(key, newValue)
                    toneLowKnob.value = newValue
                }

                onCommitRequested: reverb.commitSettings()
            }

            ParameterKnob {
                id: toneHighKnob

                Layout.fillWidth: true

                navigation.panel: root.rightColumnNavigationPanel
                navigation.order: toneLowKnob.navigation.order + 1

                parameter: reverb.paramsList["ToneHigh"]

                onNewValueRequested: function (key, newValue) {
                    newParameterValueRequested(key, newValue)
                    toneHighKnob.value = newValue
                }

                onCommitRequested: reverb.commitSettings()
            }

            SeparatorLine {
                Layout.columnSpan: 2
            }

            ParameterKnob {
                id: wetGainKnob

                Layout.fillWidth: true

                navigation.panel: root.rightColumnNavigationPanel
                navigation.order: toneHighKnob.navigation.order + 1

                parameter: reverb.paramsList["WetGain"]

                onNewValueRequested: function (key, newValue) {
                    newParameterValueRequested(key, newValue)
                    wetGainKnob.value = newValue
                }

                onCommitRequested: reverb.commitSettings()
            }

            ParameterKnob {
                id: dryGainKnob

                Layout.fillWidth: true

                navigation.panel: root.rightColumnNavigationPanel
                navigation.order: wetGainKnob.navigation.order + 1

                parameter: reverb.paramsList["DryGain"]

                onNewValueRequested: function (key, newValue) {
                    newParameterValueRequested(key, newValue)
                    dryGainKnob.value = newValue
                }

                onCommitRequested: reverb.commitSettings()
            }

            CheckBox {
                id: wetOnly

                navigation.panel: root.rightColumnNavigationPanel
                navigation.order: dryGainKnob.navigation.order + 1

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
