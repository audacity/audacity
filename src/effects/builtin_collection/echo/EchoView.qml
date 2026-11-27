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

        Repeater {
            model: ["Delay", "Decay"]

            delegate: ColumnLayout {
                id: setting

                required property string modelData
                required property int index

                readonly property var settingModel: EchoSettingModelFactory.createModel(root, root.instanceId, modelData)

                Layout.fillWidth: true
                Layout.preferredWidth: 1
                spacing: prv.labelSpacing

                Component.onCompleted: {
                    settingModel.init()
                }

                StyledTextLabel {
                    Layout.fillWidth: true
                    horizontalAlignment: Text.AlignLeft
                    text: setting.settingModel.title
                }

                IncrementalPropertyControl {
                    Layout.fillWidth: true

                    navigation.panel: root.echoNavigationPanel
                    navigation.order: setting.index

                    currentValue: setting.settingModel.value
                    measureUnitsSymbol: setting.settingModel.unit
                    minValue: setting.settingModel.min
                    maxValue: setting.settingModel.max
                    decimals: setting.settingModel.decimals
                    step: setting.settingModel.step

                    onValueEdited: function (newValue) {
                        setting.settingModel.value = newValue
                    }
                }
            }
        }
    }
}
