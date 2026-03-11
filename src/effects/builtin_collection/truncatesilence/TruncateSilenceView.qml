/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts
import Muse.Ui 1.0
import Muse.UiComponents
import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection

BuiltinEffectBase {
    id: root

    width: prv.desiredWidth - (2 * prv.spaceXL) // we need to remove the padding from the dialog desired width
    implicitHeight: mainColumn.height // see with DestructiveEffectsViewerDialog.qml

    property string title: truncateSilence.effectTitle()
    property bool isApplyAllowed: true

    builtinEffectModel: TruncateSilenceViewModelFactory.createModel(root, root.instanceId)
    property alias truncateSilence: root.builtinEffectModel
    property NavigationPanel detectSilenceNavigationPanel: NavigationPanel {
        name: "TruncateSilenceDetectSilence"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }
    property NavigationPanel actionNavigationPanel: NavigationPanel {
        name: "TruncateSilenceAction"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 2
    }

    QtObject {
        id: prv

        readonly property int spaceS: 4
        readonly property int spaceM: 8
        readonly property int spaceXL: 16

        readonly property int fieldWidth: 156
        readonly property int desiredWidth: 360

        readonly property int borderWidth: 1
        readonly property int borderRadius: 4
    }

    ColumnLayout {
        id: mainColumn

        width: parent.width
        spacing: prv.spaceXL

        // Detect silence section
        Column {

            Layout.fillWidth: true

            spacing: prv.spaceM

            StyledTextLabel {

                text: truncateSilence.detectSilenceLabel()
                font: ui.theme.bodyFont
            }

            RoundedRectangle {

                width: parent.width
                height: detectSilenceRow.height + prv.spaceXL * 2

                color: ui.theme.backgroundSecondaryColor
                radius: prv.borderRadius

                border.color: ui.theme.strokeColor
                border.width: prv.borderWidth

                Row {
                    id: detectSilenceRow

                    anchors.centerIn: parent

                    width: parent.width - prv.spaceXL * 2

                    spacing: prv.spaceXL

                    Column {

                        width: (parent.width - parent.spacing) / 2

                        spacing: prv.spaceM

                        StyledTextLabel {

                            text: truncateSilence.thresholdLabel()
                        }

                        IncrementalPropertyControl {
                            id: thresholdControl

                            width: parent.width

                            navigation.panel: root.detectSilenceNavigationPanel
                            navigation.order: 0

                            currentValue: truncateSilence.thresholdValue
                            measureUnitsSymbol: truncateSilence.thresholdUnitSymbol()
                            decimals: truncateSilence.thresholdDecimals()
                            step: truncateSilence.thresholdStep()
                            minValue: truncateSilence.thresholdMin()
                            maxValue: truncateSilence.thresholdMax()

                            onValueEdited: function (newValue) {
                                truncateSilence.thresholdValue = newValue
                            }
                        }
                    }

                    Column {

                        width: (parent.width - parent.spacing) / 2

                        spacing: prv.spaceM

                        StyledTextLabel {

                            text: truncateSilence.minimumLabel()
                        }

                        IncrementalPropertyControl {
                            id: minimumControl

                            width: parent.width

                            navigation.panel: root.detectSilenceNavigationPanel
                            navigation.order: thresholdControl.navigation.order + 1

                            currentValue: truncateSilence.minimumValue
                            measureUnitsSymbol: truncateSilence.minimumUnitSymbol()
                            decimals: truncateSilence.minimumDecimals()
                            step: truncateSilence.minimumStep()
                            minValue: truncateSilence.minimumMin()
                            maxValue: truncateSilence.minimumMax()

                            onValueEdited: function (newValue) {
                                truncateSilence.minimumValue = newValue
                            }
                        }
                    }
                }
            }
        }

        // Action section
        Column {

            Layout.fillWidth: true

            spacing: prv.spaceM

            StyledTextLabel {

                text: truncateSilence.actionLabel()
                font: ui.theme.bodyFont
            }

            RoundedRectangle {

                width: parent.width
                height: actionColumn.height + prv.spaceXL * 2

                color: ui.theme.backgroundSecondaryColor
                radius: prv.borderRadius

                border.color: ui.theme.strokeColor
                border.width: prv.borderWidth

                Column {
                    id: actionColumn

                    anchors.centerIn: parent

                    width: parent.width - prv.spaceXL * 2

                    spacing: prv.spaceXL

                    RadioButtonGroup {

                        width: parent.width

                        spacing: prv.spaceM
                        orientation: ListView.Vertical

                        model: truncateSilence.actionModel()

                        delegate: RoundedRadioButton {
                            text: modelData.text
                            checked: truncateSilence.actionIndex === modelData.value

                            navigation.panel: root.actionNavigationPanel
                            navigation.order: index

                            onToggled: {
                                truncateSilence.actionIndex = modelData.value
                            }
                        }
                    }

                    // Dynamic action field based on current selection
                    Column {

                        width: parent.width

                        spacing: prv.spaceS

                        StyledTextLabel {

                            text: truncateSilence.currentActionConfig.fieldLabel || ""
                        }

                        IncrementalPropertyControl {
                            id: actionValueControl

                            width: prv.fieldWidth

                            navigation.panel: root.actionNavigationPanel
                            navigation.order: truncateSilence.actionModel().length

                            currentValue: truncateSilence.actionIndex === 0 ? truncateSilence.truncateValue : truncateSilence.compressValue
                            measureUnitsSymbol: truncateSilence.currentActionConfig.paramUnitSymbol || ""
                            decimals: truncateSilence.currentActionConfig.paramDecimals || 0
                            step: truncateSilence.currentActionConfig.paramStep || 1
                            minValue: truncateSilence.currentActionConfig.paramMin || 0
                            maxValue: truncateSilence.currentActionConfig.paramMax || 100

                            onValueEdited: function (newValue) {
                                if (truncateSilence.actionIndex === 0) {
                                    truncateSilence.truncateValue = newValue
                                } else {
                                    truncateSilence.compressValue = newValue
                                }
                            }
                        }
                    }

                    CheckBox {
                        id: independentCheckbox

                        width: parent.width

                        navigation.panel: root.actionNavigationPanel
                        navigation.order: actionValueControl.navigation.order + 1

                        text: truncateSilence.currentActionConfig.independentLabel || ""
                        checked: truncateSilence.independentValue

                        onClicked: {
                            truncateSilence.independentValue = !checked
                        }
                    }
                }
            }
        }
    }
}
