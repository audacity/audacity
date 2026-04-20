import QtQuick
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection

BuiltinEffectBase {
    id: root

    property string title: qsTrc("effects/paulstretch", "Paulstretch")
    property bool isApplyAllowed: true

    width: 420
    implicitHeight: row.height
    numNavigationPanels: 1

    builtinEffectModel: PaulstretchViewModelFactory.createModel(root, root.instanceId)
    property alias paulstretch: root.builtinEffectModel
    property NavigationPanel paulstretchNavigationPanel: NavigationPanel {
        name: "PaulstretchControls"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }

    RowLayout {
        id: row

        width: parent.width
        spacing: 24

        ColumnLayout {
            Layout.fillWidth: true
            Layout.preferredWidth: 1
            spacing: 8

            StyledTextLabel {
                Layout.fillWidth: true
                horizontalAlignment: Text.AlignLeft
                text: paulstretch.stretchFactorLabel
            }

            IncrementalPropertyControl {
                id: stretchFactorControl

                Layout.fillWidth: true

                navigation.panel: root.paulstretchNavigationPanel
                navigation.order: 0

                currentValue: paulstretch.amount
                minValue: paulstretch.stretchFactorMin
                maxValue: paulstretch.stretchFactorMax
                decimals: paulstretch.stretchFactorDecimals
                step: paulstretch.stretchFactorStep

                onValueEdited: function (newValue) {
                    paulstretch.amount = newValue
                }
            }
        }

        ColumnLayout {
            Layout.fillWidth: true
            Layout.preferredWidth: 1
            spacing: 8

            StyledTextLabel {
                Layout.fillWidth: true
                horizontalAlignment: Text.AlignLeft
                text: paulstretch.timeResolutionLabel
            }

            IncrementalPropertyControl {
                id: timeResolutionControl

                Layout.fillWidth: true

                navigation.panel: root.paulstretchNavigationPanel
                navigation.order: stretchFactorControl.navigation.order + 1

                currentValue: paulstretch.timeResolution
                measureUnitsSymbol: paulstretch.timeResolutionUnitSymbol
                minValue: paulstretch.timeResolutionMin
                maxValue: paulstretch.timeResolutionMax
                decimals: paulstretch.timeResolutionDecimals
                step: paulstretch.timeResolutionStep

                onValueEdited: function (newValue) {
                    paulstretch.timeResolution = newValue
                }
            }
        }
    }
}
