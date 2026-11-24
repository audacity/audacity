import QtQuick
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection

BuiltinEffectBase {
    id: root

    //: Title of the Echo effect dialog
    property string title: qsTrc("effects/echo", "Echo")
    property bool isApplyAllowed: true

    width: prv.contentWidth
    implicitHeight: row.height + prv.focusBorderSpace
    numNavigationPanels: 1

    QtObject {
        id: prv

        readonly property int contentWidth: 420
        readonly property int columnSpacing: 24
        readonly property int labelSpacing: 8
        readonly property real focusBorderSpace: ui.theme.navCtrlBorderWidth
    }

    builtinEffectModel: EchoViewModelFactory.createModel(root, root.instanceId)
    property alias echo: root.builtinEffectModel
    property NavigationPanel echoNavigationPanel: NavigationPanel {
        name: "EchoControls"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }

    RowLayout {
        id: row

        width: parent.width
        spacing: prv.columnSpacing

        ColumnLayout {
            Layout.fillWidth: true
            Layout.preferredWidth: 1
            spacing: prv.labelSpacing

            StyledTextLabel {
                Layout.fillWidth: true
                horizontalAlignment: Text.AlignLeft
                text: echo.delayLabel
            }

            IncrementalPropertyControl {
                id: delayControl

                Layout.fillWidth: true

                navigation.panel: root.echoNavigationPanel
                navigation.order: 0

                currentValue: echo.delay
                measureUnitsSymbol: echo.delayUnitSymbol
                minValue: echo.delayMin
                maxValue: echo.delayMax
                decimals: echo.delayDecimals
                step: echo.delayStep

                onValueEdited: function (newValue) {
                    echo.delay = newValue
                }
            }
        }

        ColumnLayout {
            Layout.fillWidth: true
            Layout.preferredWidth: 1
            spacing: prv.labelSpacing

            StyledTextLabel {
                Layout.fillWidth: true
                horizontalAlignment: Text.AlignLeft
                text: echo.decayLabel
            }

            IncrementalPropertyControl {
                id: decayControl

                Layout.fillWidth: true

                navigation.panel: root.echoNavigationPanel
                navigation.order: delayControl.navigation.order + 1

                currentValue: echo.decay
                minValue: echo.decayMin
                maxValue: echo.decayMax
                decimals: echo.decayDecimals
                step: echo.decayStep

                onValueEdited: function (newValue) {
                    echo.decay = newValue
                }
            }
        }
    }
}
