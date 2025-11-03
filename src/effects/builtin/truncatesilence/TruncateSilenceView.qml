/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts
import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Audacity.Effects
import Audacity.BuiltinEffects

BuiltinEffectBase {
    id: root

    width: prv.desiredWidth - (2 * prv.spacingL) // we need to remove the padding from the dialog desired width
    implicitHeight: mainColumn.height // see with EffectsViewerDialog.qml

    property string title: truncateSilence.effectTitle()
    property bool isApplyAllowed: true

    model: truncateSilence

    TruncateSilenceViewModel {
        id: truncateSilence
    }

    QtObject {
        id: prv

        readonly property int spacingS: 4
        readonly property int spacingM: 8
        readonly property int spacingL: 16

        readonly property int fieldWidth: 156
        readonly property int desiredWidth: 360

        readonly property int borderWidth: 1
        readonly property int borderRadius: 4
    }

    ColumnLayout {
        id: mainColumn

        width: parent.width
        spacing: prv.spacingL

        // Detect silence section
        Column {

            Layout.fillWidth: true

            spacing: prv.spacingM

            StyledTextLabel {

                text: truncateSilence.detectSilenceLabel()
                font: ui.theme.bodyFont
            }

            RoundedRectangle {

                width: parent.width
                height: detectSilenceRow.height + prv.spacingL * 2

                color: ui.theme.backgroundSecondaryColor
                radius: prv.borderRadius

                border.color: ui.theme.strokeColor
                border.width: prv.borderWidth

                Row {
                    id: detectSilenceRow

                    anchors.centerIn: parent

                    width: parent.width - prv.spacingL * 2

                    spacing: prv.spacingL

                    Column {

                        width: (parent.width - parent.spacing) / 2

                        spacing: prv.spacingM

                        StyledTextLabel {

                            text: truncateSilence.thresholdLabel()
                        }

                        IncrementalPropertyControl {

                            width: parent.width

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

                        spacing: prv.spacingM

                        StyledTextLabel {

                            text: truncateSilence.minimumLabel()
                        }

                        IncrementalPropertyControl {

                            width: parent.width

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

            spacing: prv.spacingM

            StyledTextLabel {

                text: truncateSilence.actionLabel()
                font: ui.theme.bodyFont
            }

            RoundedRectangle {

                width: parent.width
                height: actionColumn.height + prv.spacingL * 2

                color: ui.theme.backgroundSecondaryColor
                radius: prv.borderRadius

                border.color: ui.theme.strokeColor
                border.width: prv.borderWidth

                Column {
                    id: actionColumn

                    anchors.centerIn: parent

                    width: parent.width - prv.spacingL * 2

                    spacing: prv.spacingL

                    RadioButtonGroup {

                        width: parent.width

                        spacing: prv.spacingM
                        orientation: ListView.Vertical

                        model: truncateSilence.actionModel()

                        delegate: RoundedRadioButton {

                            text: modelData.text
                            checked: truncateSilence.actionIndex === modelData.value

                            onToggled: {
                                truncateSilence.actionIndex = modelData.value
                            }
                        }
                    }

                    // Dynamic action field based on current selection
                    Column {

                        width: parent.width

                        spacing: prv.spacingS

                        StyledTextLabel {

                            text: truncateSilence.currentActionConfig.fieldLabel || ""
                        }

                        IncrementalPropertyControl {

                            width: prv.fieldWidth

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

                        width: parent.width

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
