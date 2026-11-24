import QtQuick
import QtQuick.Layouts

import Muse.UiComponents

import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection

BuiltinEffectBase {
    id: root

    //: Title of the Wahwah effect dialog
    property string title: qsTrc("effects/wahwah", "Wahwah")
    property bool isApplyAllowed: true

    implicitWidth: row.implicitWidth
    implicitHeight: row.implicitHeight

    builtinEffectModel: WahWahViewModelFactory.createModel(root, root.instanceId)
    numNavigationPanels: 2
    property alias wahwah: root.builtinEffectModel

    property NavigationPanel mainNavigationPanel: NavigationPanel {
        name: "WahWahMain"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }

    property NavigationPanel secondaryNavigationPanel: NavigationPanel {
        name: "WahWahSecondary"
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

    function newParameterValueRequested(key, value) {
        wahwah.setParam(key, value)
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

                BigParameterKnob {
                    id: freqKnob

                    Layout.fillWidth: true

                    navigation.panel: root.mainNavigationPanel
                    navigation.order: 0

                    parameter: wahwah.paramsList["Freq"]
                    defaultValue: parameter ? parameter["defaultValue"] : 0
                    radius: prv.knobRadius

                    onNewValueRequested: function (key, newValue) {
                        newParameterValueRequested(key, newValue)
                    }

                    onCommitRequested: wahwah.commitSettings()
                }

                BigParameterKnob {
                    id: depthKnob

                    Layout.fillWidth: true

                    navigation.panel: root.mainNavigationPanel
                    navigation.order: freqKnob.navigation.order + 1

                    parameter: wahwah.paramsList["Depth"]
                    defaultValue: parameter ? parameter["defaultValue"] : 0
                    radius: prv.knobRadius

                    onNewValueRequested: function (key, newValue) {
                        newParameterValueRequested(key, newValue)
                    }

                    onCommitRequested: wahwah.commitSettings()
                }

                BigParameterKnob {
                    id: resonanceKnob

                    Layout.fillWidth: true

                    navigation.panel: root.mainNavigationPanel
                    navigation.order: depthKnob.navigation.order + 1

                    parameter: wahwah.paramsList["Resonance"]
                    defaultValue: parameter ? parameter["defaultValue"] : 0
                    radius: prv.knobRadius

                    onNewValueRequested: function (key, newValue) {
                        newParameterValueRequested(key, newValue)
                    }

                    onCommitRequested: wahwah.commitSettings()
                }

                BigParameterKnob {
                    id: offsetKnob

                    Layout.fillWidth: true

                    navigation.panel: root.mainNavigationPanel
                    navigation.order: resonanceKnob.navigation.order + 1

                    parameter: wahwah.paramsList["Offset"]
                    defaultValue: parameter ? parameter["defaultValue"] : 0
                    radius: prv.knobRadius

                    onNewValueRequested: function (key, newValue) {
                        newParameterValueRequested(key, newValue)
                    }

                    onCommitRequested: wahwah.commitSettings()
                }
            }
        }

        ColumnLayout {
            Layout.fillHeight: true
            Layout.fillWidth: true

            spacing: prv.narrowRowSpacing

            ParameterKnob {
                id: phaseKnob

                Layout.fillWidth: true

                navigation.panel: root.secondaryNavigationPanel
                navigation.order: 0

                parameter: wahwah.paramsList["Phase"]
                defaultValue: parameter ? parameter["defaultValue"] : 0

                onNewValueRequested: function (key, newValue) {
                    newParameterValueRequested(key, newValue)
                }

                onCommitRequested: wahwah.commitSettings()
            }

            SeparatorLine {}

            ParameterKnob {
                id: gainKnob

                Layout.fillWidth: true

                navigation.panel: root.secondaryNavigationPanel
                navigation.order: phaseKnob.navigation.order + 1

                parameter: wahwah.paramsList["Gain"]
                defaultValue: parameter ? parameter["defaultValue"] : 0

                onNewValueRequested: function (key, newValue) {
                    newParameterValueRequested(key, newValue)
                }

                onCommitRequested: wahwah.commitSettings()
            }
        }
    }
}
