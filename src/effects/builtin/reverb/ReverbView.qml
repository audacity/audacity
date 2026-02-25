import QtQuick 2.15
import QtQuick.Layouts

import Muse.UiComponents

import Audacity.Effects
import Audacity.BuiltinEffects

BuiltinEffectBase {
    id: root

    property string title: qsTrc("effects/reverb", "Reverb")
    property bool isApplyAllowed: true

    implicitHeight: row.height
    width: 560

    builtinEffectModel: ReverbViewModelFactory.createModel(root, root.instanceId)
    property alias reverb: root.builtinEffectModel

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

                    navigationPanel: root.navigationPanel
                    navigationOrder: root.navigationOrder

                    parameter: reverb.paramsList["RoomSize"]
                    radius: 24

                    onNewValueRequested: function (key, newValue) {
                        newParameterValueRequested(key, newValue)
                        value = newValue
                    }

                    onCommitRequested: reverb.commitSettings()
                }

                BigParameterKnob {
                    id: stereoWidthKnob

                    Layout.fillWidth: true

                    navigationPanel: root.navigationPanel
                    navigationOrder: roomSizeKnob.navigationOrderEnd

                    parameter: reverb.paramsList["StereoWidth"]
                    radius: 24

                    onNewValueRequested: function (key, newValue) {
                        newParameterValueRequested(key, newValue)
                        value = newValue
                    }

                    onCommitRequested: reverb.commitSettings()
                }

                BigParameterKnob {
                    Layout.fillWidth: true

                    navigationPanel: root.navigationPanel
                    navigationOrder: stereoWidthKnob.navigationOrderEnd

                    parameter: reverb.paramsList["PreDelay"]
                    radius: 24

                    onNewValueRequested: function (key, newValue) {
                        newParameterValueRequested(key, newValue)
                        value = newValue
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

                navigationPanel: root.navigationPanel
                navigationOrder: leftColumn.navigationOrderEnd

                parameter: reverb.paramsList["HfDamping"]

                onNewValueRequested: function (key, newValue) {
                    newParameterValueRequested(key, newValue)
                    value = newValue
                }

                onCommitRequested: reverb.commitSettings()
            }

            ParameterKnob {
                id: reverberanceKnob

                Layout.fillWidth: true

                navigationPanel: root.navigationPanel
                navigationOrder: hfDampingKnob.navigationOrderEnd

                parameter: reverb.paramsList["Reverberance"]

                onNewValueRequested: function (key, newValue) {
                    newParameterValueRequested(key, newValue)
                    value = newValue
                }

                onCommitRequested: reverb.commitSettings()
            }

            ParameterKnob {
                id: toneLowKnob

                Layout.fillWidth: true

                navigationPanel: root.navigationPanel
                navigationOrder: reverberanceKnob.navigationOrderEnd

                parameter: reverb.paramsList["ToneLow"]

                onNewValueRequested: function (key, newValue) {
                    newParameterValueRequested(key, newValue)
                    value = newValue
                }

                onCommitRequested: reverb.commitSettings()
            }

            ParameterKnob {
                id: toneHighKnob

                Layout.fillWidth: true

                navigationPanel: root.navigationPanel
                navigationOrder: toneLowKnob.navigationOrderEnd

                parameter: reverb.paramsList["ToneHigh"]

                onNewValueRequested: function (key, newValue) {
                    newParameterValueRequested(key, newValue)
                    value = newValue
                }

                onCommitRequested: reverb.commitSettings()
            }

            SeparatorLine {
                Layout.columnSpan: 2
            }

            ParameterKnob {
                id: wetGainKnob

                Layout.fillWidth: true

                navigationPanel: root.navigationPanel
                navigationOrder: toneHighKnob.navigationOrderEnd

                parameter: reverb.paramsList["WetGain"]

                onNewValueRequested: function (key, newValue) {
                    newParameterValueRequested(key, newValue)
                    value = newValue
                }

                onCommitRequested: reverb.commitSettings()
            }

            ParameterKnob {
                id: dryGainKnob

                Layout.fillWidth: true

                navigationPanel: root.navigationPanel
                navigationOrder: wetGainKnob.navigationOrderEnd

                parameter: reverb.paramsList["DryGain"]

                onNewValueRequested: function (key, newValue) {
                    newParameterValueRequested(key, newValue)
                    value = newValue
                }

                onCommitRequested: reverb.commitSettings()
            }

            CheckBox {
                id: wetOnly

                navigation.panel: root.navigationPanel
                navigation.order: dryGainKnob.navigationOrderEnd

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
