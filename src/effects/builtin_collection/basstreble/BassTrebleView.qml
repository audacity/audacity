import QtQuick 2.15
import QtQuick.Layouts

import Muse.UiComponents

import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection

BuiltinEffectBase {
    id: root

    property string title: qsTrc("effects/basstreble", "Bass and Treble")
    property bool isApplyAllowed: true

    implicitWidth: column.implicitWidth
    implicitHeight: column.implicitHeight

    builtinEffectModel: BassTrebleViewModelFactory.createModel(root, root.instanceId)
    numNavigationPanels: 3
    property alias bassTreble: root.builtinEffectModel

    property NavigationPanel leftColumnNavigationPanel: NavigationPanel {
        name: "BassTrebleLeftColumn"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }

    property NavigationPanel rightColumnNavigationPanel: NavigationPanel {
        name: "BassTrebleRightColumn"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 2
    }

    property NavigationPanel linkNavigationPanel: NavigationPanel {
        name: "BassTrebleLink"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 3
    }

    QtObject {
        id: prv

        readonly property int innerKnobSpacing: 48
        readonly property int gridSpacing: 16
        readonly property int rowSpacing: 16

        readonly property int verticalPadding: 24
        readonly property int horizontalLeftColumnPadding: 42
        readonly property int horizontalRightColumnPadding: 16

        readonly property int knobRadius: 24

        readonly property string linkCheckboxText: qsTrc("effects/basstreble", "Auto-adjust volume to preserve loudness")
    }

    function newParameterValueRequested(key, value) {
        bassTreble.setParam(key, value)
    }

    ColumnLayout {
        id: column

        width: parent.width
        spacing: prv.rowSpacing

        RowLayout {
            spacing: prv.gridSpacing

            RoundedRectangle {
                Layout.preferredWidth: leftColumn.implicitWidth + 2 * prv.horizontalLeftColumnPadding
                Layout.preferredHeight: leftColumn.implicitHeight + 2 * prv.verticalPadding

                color: ui.theme.backgroundSecondaryColor

                border.color: ui.theme.strokeColor
                border.width: 1

                radius: 4

                GridLayout {
                    id: leftColumn

                    anchors.centerIn: parent

                    columns: 2
                    columnSpacing: prv.innerKnobSpacing

                    BigParameterKnob {
                        id: bassKnob

                        Layout.fillWidth: true

                        navigation.panel: root.leftColumnNavigationPanel
                        navigation.order: 0

                        parameter: bassTreble.paramsList["Bass"]
                        radius: prv.knobRadius

                        onNewValueRequested: function (key, newValue) {
                            newParameterValueRequested(key, newValue)
                        }

                        onCommitRequested: bassTreble.commitSettings()
                    }

                    BigParameterKnob {
                        id: trebleKnob

                        Layout.fillWidth: true

                        navigation.panel: root.leftColumnNavigationPanel
                        navigation.order: bassKnob.navigation.order + 1

                        parameter: bassTreble.paramsList["Treble"]
                        radius: prv.knobRadius

                        onNewValueRequested: function (key, newValue) {
                            newParameterValueRequested(key, newValue)
                        }

                        onCommitRequested: bassTreble.commitSettings()
                    }
                }
            }

            RoundedRectangle {
                Layout.preferredWidth: rightColumn.implicitWidth + 2 * prv.horizontalRightColumnPadding
                Layout.preferredHeight: leftColumn.implicitHeight + 2 * prv.verticalPadding

                color: ui.theme.backgroundSecondaryColor

                border.color: ui.theme.strokeColor
                border.width: 1

                radius: 4

                GridLayout {
                    id: rightColumn

                    anchors.centerIn: parent

                    columns: 1

                    BigParameterKnob {
                        id: gainKnob

                        Layout.fillWidth: true

                        navigation.panel: root.rightColumnNavigationPanel
                        navigation.order: 0

                        parameter: bassTreble.paramsList["Gain"]
                        radius: prv.knobRadius

                        onNewValueRequested: function (key, newValue) {
                            newParameterValueRequested(key, newValue)
                        }

                        onCommitRequested: bassTreble.commitSettings()
                    }
                }
            }
        }

        CheckBox {
            id: linkCheckbox

            Layout.bottomMargin: root.usedDestructively ? prv.gridSpacing : 0

            navigation.panel: root.linkNavigationPanel
            navigation.order: 0

            text: prv.linkCheckboxText
            checked: bassTreble.link
            onClicked: function () {
                bassTreble.link = !bassTreble.link
                bassTreble.commitSettings()
            }
        }
    }
}
