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

    property string title: truncateSilence.effectTitle
    property bool isApplyAllowed: true

    implicitHeight: mainColumn.height // see with EffectsViewerDialog.qml

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

    width: prv.desiredWidth - (2 * prv.spacingL) // we need to remove the padding from the dialog desired width

    ColumnLayout {
        id: mainColumn

        width: parent.width
        spacing: prv.spacingL

        // Detect silence section
        Column {
            Layout.fillWidth: true
            spacing: prv.spacingM

            StyledTextLabel {
                text: truncateSilence.detectSilenceLabel
                font: ui.theme.bodyFont
            }

            RoundedRectangle {
                width: parent.width
                height: detectSilenceRow.height + prv.spacingL * 2

                color: ui.theme.backgroundSecondaryColor

                border.color: ui.theme.strokeColor
                border.width: prv.borderWidth

                radius: prv.borderRadius

                Row {
                    id: detectSilenceRow
                    anchors.centerIn: parent
                    width: parent.width - prv.spacingL * 2
                    spacing: prv.spacingL

                    Column {
                        width: (parent.width - parent.spacing) / 2
                        spacing: prv.spacingM

                        StyledTextLabel {
                            text: truncateSilence.thresholdLabel
                        }

                        TextInputField {
                            width: parent.width
                            currentText: truncateSilence.thresholdValue.toFixed(truncateSilence.thresholdDecimals) + " " + truncateSilence.dbUnit
                            onTextEditingFinished: function (newTextValue) {
                                var val = parseFloat(newTextValue)
                                if (!isNaN(val)) {
                                    truncateSilence.thresholdValue = val
                                }
                            }
                        }
                    }

                    Column {
                        width: (parent.width - parent.spacing) / 2
                        spacing: prv.spacingM

                        StyledTextLabel {
                            text: truncateSilence.minimumLabel
                        }

                        TextInputField {
                            width: parent.width
                            currentText: truncateSilence.minimumValue.toFixed(truncateSilence.minimumDecimals) + " " + truncateSilence.secondsUnit
                            onTextEditingFinished: function (newTextValue) {
                                var val = parseFloat(newTextValue)
                                if (!isNaN(val)) {
                                    truncateSilence.minimumValue = val
                                }
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
                text: truncateSilence.actionLabel
                font: ui.theme.bodyFont
            }

            RoundedRectangle {
                width: parent.width
                height: actionColumn.height + prv.spacingL * 2

                color: ui.theme.backgroundSecondaryColor

                border.color: ui.theme.strokeColor
                border.width: prv.borderWidth

                radius: prv.borderRadius

                Column {
                    id: actionColumn
                    anchors.centerIn: parent
                    width: parent.width - prv.spacingL * 2
                    spacing: prv.spacingL
                    Column {
                        width: parent.width
                        spacing: prv.spacingM
                        RoundedRadioButton {
                            text: truncateSilence.truncateActionLabel
                            checked: truncateSilence.actionIndex === 0
                            onToggled: {
                                if (truncateSilence.actionIndex !== 0) {
                                    truncateSilence.actionIndex = 0
                                }
                            }
                        }

                        RoundedRadioButton {
                            text: truncateSilence.compressActionLabel
                            checked: truncateSilence.actionIndex === 1
                            onToggled: {
                                if (truncateSilence.actionIndex !== 1) {
                                    truncateSilence.actionIndex = 1
                                }
                            }
                        }
                    }

                    // Truncate to field (visible when action = 0)
                    Column {
                        width: parent.width
                        spacing: prv.spacingS
                        visible: truncateSilence.actionIndex === 0

                        StyledTextLabel {
                            text: truncateSilence.truncateToLabel
                        }

                        TextInputField {
                            width: prv.fieldWidth
                            currentText: truncateSilence.truncateValue.toFixed(truncateSilence.truncateDecimals) + " " + truncateSilence.secondsUnit
                            onTextEditingFinished: function (newTextValue) {
                                var val = parseFloat(newTextValue)
                                if (!isNaN(val)) {
                                    truncateSilence.truncateValue = val
                                }
                            }
                        }
                    }

                    // Compress to field (visible when action = 1)
                    Column {
                        width: parent.width
                        spacing: prv.spacingS
                        visible: truncateSilence.actionIndex === 1

                        StyledTextLabel {
                            text: truncateSilence.compressToLabel
                        }

                        TextInputField {
                            width: prv.fieldWidth
                            currentText: truncateSilence.compressValue.toFixed(truncateSilence.compressDecimals) + " " + truncateSilence.percentUnit
                            onTextEditingFinished: function (newTextValue) {
                                var val = parseFloat(newTextValue)
                                if (!isNaN(val)) {
                                    truncateSilence.compressValue = val
                                }
                            }
                        }
                    }

                    CheckBox {
                        width: parent.width
                        text: truncateSilence.actionIndex === 0 ? truncateSilence.independentTruncateLabel : truncateSilence.independentCompressLabel
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
