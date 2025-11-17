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

    width: prv.desiredWidth - (2 * prv.spaceXL) // we need to remove the padding from the dialog desired width
    implicitHeight: mainColumn.height // see with EffectsViewerDialog.qml

    property string title: changePitch.effectTitle()
    property bool isApplyAllowed: true

    model: changePitch

    ChangePitchViewModel {
        id: changePitch
    }

    QtObject {
        id: prv

        readonly property int spaceS: 4
        readonly property int spaceM: 8
        readonly property int spaceXL: 16

        readonly property int pitchDropdownFieldWidth: 128
        readonly property int fieldWidth: 98
        readonly property int desiredWidth: 480

        readonly property int borderWidth: 1
        readonly property int borderRadius: 4
    }

    ColumnLayout {
        id: mainColumn

        width: parent.width
        spacing: prv.spaceXL

        // Estimated start pitch label
        StyledTextLabel {
            Layout.fillWidth: true
            horizontalAlignment: Text.AlignLeft
            text: changePitch.estimatedStartPitch
            font: ui.theme.bodyFont
        }

        // From pitch / To pitch section
        Column {
            Layout.fillWidth: true
            spacing: prv.spaceM

            RoundedRectangle {
                width: parent.width
                height: pitchRow.height + prv.spaceXL * 2

                color: ui.theme.backgroundSecondaryColor
                radius: prv.borderRadius

                border.color: ui.theme.strokeColor
                border.width: prv.borderWidth

                Row {
                    id: pitchRow

                    anchors.centerIn: parent
                    width: parent.width - prv.spaceXL * 2
                    spacing: prv.spaceXL

                    // From pitch column
                    Column {
                        width: (parent.width - parent.spacing) / 2
                        spacing: prv.spaceS

                        StyledTextLabel {
                            text: changePitch.fromPitchLabel()
                        }

                        Row {
                            width: parent.width
                            spacing: prv.spaceM

                            StyledDropdown {
                                id: fromPitchDropdown
                                width: prv.pitchDropdownFieldWidth

                                currentIndex: changePitch.fromPitchValue
                                model: changePitch.fromPitchModel()

                                onActivated: function (index, value) {
                                    changePitch.fromPitchValue = index
                                }
                            }

                            IncrementalPropertyControl {
                                id: fromOctaveControl
                                width: parent.width - fromPitchDropdown.width - parent.spacing
                                currentValue: changePitch.fromOctaveValue
                                measureUnitsSymbol: changePitch.fromOctaveUnitSymbol()
                                decimals: changePitch.fromOctaveDecimals()
                                step: changePitch.fromOctaveStep()
                                minValue: changePitch.fromOctaveMin()
                                maxValue: changePitch.fromOctaveMax()

                                onValueEdited: function (newValue) {
                                    changePitch.fromOctaveValue = newValue
                                }
                            }
                        }
                    }

                    // To pitch column
                    Column {
                        width: (parent.width - parent.spacing) / 2
                        spacing: prv.spaceS

                        StyledTextLabel {
                            text: changePitch.toPitchLabel()
                        }

                        Row {
                            width: parent.width
                            spacing: prv.spaceM

                            StyledDropdown {
                                id: toPitchDropdown
                                width: prv.pitchDropdownFieldWidth

                                currentIndex: changePitch.toPitchValue
                                model: changePitch.toPitchModel()

                                onActivated: function (index, value) {
                                    changePitch.toPitchValue = index
                                }
                            }

                            IncrementalPropertyControl {
                                id: toOctaveControl
                                width: parent.width - fromPitchDropdown.width - parent.spacing
                                currentValue: changePitch.toOctaveValue
                                measureUnitsSymbol: changePitch.toOctaveUnitSymbol()
                                decimals: changePitch.toOctaveDecimals()
                                step: changePitch.toOctaveStep()
                                minValue: changePitch.toOctaveMin()
                                maxValue: changePitch.toOctaveMax()

                                onValueEdited: function (newValue) {
                                    changePitch.toOctaveValue = newValue
                                }
                            }
                        }
                    }
                }
            }
        }

        // Semitones / Cents section
        Column {
            Layout.fillWidth: true
            spacing: prv.spaceM

            RoundedRectangle {
                width: parent.width
                height: semitonesRow.height + prv.spaceXL * 2

                color: ui.theme.backgroundSecondaryColor
                radius: prv.borderRadius

                border.color: ui.theme.strokeColor
                border.width: prv.borderWidth

                Row {
                    id: semitonesRow

                    anchors.centerIn: parent
                    width: parent.width - prv.spaceXL * 2
                    spacing: prv.spaceXL

                    // Semitones column
                    Column {
                        width: (parent.width - parent.spacing) / 2
                        spacing: prv.spaceS

                        StyledTextLabel {
                            text: changePitch.semitonesLabel()
                        }

                        IncrementalPropertyControl {
                            width: parent.width
                            currentValue: changePitch.semitonesIntegerValue()
                            measureUnitsSymbol: changePitch.semitonesUnitSymbol()
                            decimals: changePitch.semitonesDecimals()
                            step: changePitch.semitonesStep()
                            minValue: changePitch.semitonesMin()
                            maxValue: changePitch.semitonesMax()

                            onValueEdited: function (newValue) {
                                changePitch.setSemitonesIntegerValue(newValue)
                            }
                        }
                    }

                    // Cents column
                    Column {
                        width: (parent.width - parent.spacing) / 2
                        spacing: prv.spaceS

                        StyledTextLabel {
                            text: changePitch.centsLabel()
                        }

                        IncrementalPropertyControl {
                            width: parent.width
                            currentValue: changePitch.centsValue()
                            measureUnitsSymbol: changePitch.centsUnitSymbol()
                            decimals: changePitch.centsDecimals()
                            step: changePitch.centsStep()
                            minValue: changePitch.centsMin()
                            maxValue: changePitch.centsMax()

                            onValueEdited: function (newValue) {
                                changePitch.setCentsValue(newValue)
                            }
                        }
                    }
                }
            }
        }

        // Frequency section
        Column {
            Layout.fillWidth: true
            spacing: prv.spaceM

            RoundedRectangle {
                width: parent.width
                height: frequencyColumn.height + prv.spaceXL * 2

                color: ui.theme.backgroundSecondaryColor
                radius: prv.borderRadius

                border.color: ui.theme.strokeColor
                border.width: prv.borderWidth

                Column {
                    id: frequencyColumn

                    anchors.centerIn: parent
                    width: parent.width - prv.spaceXL * 2
                    spacing: prv.spaceXL

                    // From frequency / To frequency row
                    Row {
                        width: parent.width
                        spacing: prv.spaceXL

                        // From frequency column
                        Column {
                            width: (parent.width - parent.spacing) / 2
                            spacing: prv.spaceS

                            StyledTextLabel {
                                text: changePitch.fromFrequencyLabel()
                            }

                            IncrementalPropertyControl {
                                width: parent.width
                                currentValue: changePitch.fromFrequencyValue
                                measureUnitsSymbol: changePitch.fromFrequencyUnitSymbol()
                                decimals: changePitch.fromFrequencyDecimals()
                                step: changePitch.fromFrequencyStep()
                                minValue: changePitch.fromFrequencyMin()
                                maxValue: changePitch.fromFrequencyMax()

                                onValueEdited: function (newValue) {
                                    changePitch.fromFrequencyValue = newValue
                                }
                            }
                        }

                        // To frequency column
                        Column {
                            width: (parent.width - parent.spacing) / 2
                            spacing: prv.spaceS

                            StyledTextLabel {
                                text: changePitch.toFrequencyLabel()
                            }

                            IncrementalPropertyControl {
                                width: parent.width
                                currentValue: changePitch.toFrequencyValue
                                measureUnitsSymbol: changePitch.toFrequencyUnitSymbol()
                                decimals: changePitch.toFrequencyDecimals()
                                step: changePitch.toFrequencyStep()
                                minValue: changePitch.toFrequencyMin()
                                maxValue: changePitch.toFrequencyMax()

                                onValueEdited: function (newValue) {
                                    changePitch.toFrequencyValue = newValue
                                }
                            }
                        }
                    }

                    // Percentage change row
                    Column {
                        width: parent.width
                        spacing: prv.spaceS

                        StyledTextLabel {
                            text: changePitch.percentChangeLabel()
                        }

                        Row {
                            width: parent.width
                            spacing: prv.spaceXL

                            StyledSlider {
                                id: percentSlider
                                anchors.verticalCenter: parent.verticalCenter

                                width: parent.width - percentField.width - parent.spacing

                                from: changePitch.percentChangeMin()
                                to: changePitch.percentChangeMax()
                                value: changePitch.percentChangeValue
                                stepSize: changePitch.percentChangeStep()

                                onMoved: {
                                    changePitch.percentChangeValue = value
                                }
                            }

                            IncrementalPropertyControl {
                                id: percentField
                                width: prv.fieldWidth
                                currentValue: changePitch.percentChangeValue
                                measureUnitsSymbol: changePitch.percentChangeUnitSymbol()
                                decimals: changePitch.percentChangeDecimals()
                                step: changePitch.percentChangeStep()
                                minValue: changePitch.percentChangeMin()
                                maxValue: changePitch.percentChangeMax()

                                onValueEdited: function (newValue) {
                                    changePitch.percentChangeValue = newValue
                                }
                            }
                        }
                    }
                }
            }
        }

        // Use high quality stretching checkbox
        CheckBox {
            Layout.fillWidth: true
            text: changePitch.useSBSMSLabel()
            checked: changePitch.useSBSMSValue
            enabled: changePitch.useSBSMSEnabled()

            onClicked: {
                changePitch.useSBSMSValue = !checked
            }
        }
    }
}
