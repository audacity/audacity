import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.UiComponents
import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection

BuiltinEffectBase {
    id: root

    property string title: qsTrc("effects/repeat", "Repeat")
    property bool isApplyAllowed: true

    // Never narrower than what the dialog's bottom buttons need, wider if a time format needs it
    readonly property int minimumContentWidth: 340

    implicitWidth: Math.max(minimumContentWidth, column.implicitWidth)
    width: implicitWidth
    implicitHeight: column.implicitHeight
    numNavigationPanels: 1

    builtinEffectModel: RepeatViewModelFactory.createModel(root, root.instanceId)
    property alias repeat: root.builtinEffectModel

    property NavigationPanel repeatNavigationPanel: NavigationPanel {
        name: "RepeatControls"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Vertical
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }

    Component.onCompleted: {
        // Focus the count with its value selected, so that typing a number and pressing Enter applies
        Qt.callLater(repeatCountControl.forceActiveFocus)
    }

    // Put a space between every value and its unit (the "seconds" format has none) and around the "+"
    // of the formats that add samples or frames, e.g. "01 h 02 m 05 s + 19580 samples"
    function spaced(timeString) {
        return timeString.replace(/(\d)(?=[A-Za-z])/g, "$1 ").replace(/\s*\+\s*/g, " + ")
    }

    // Format the lengths like the time display of the playback toolbar
    TimecodeModel {
        id: currentLengthFormatter

        mode: TimecodeModeSelector.Duration
        sampleRate: repeat.sampleRate
        tempo: repeat.tempo
        upperTimeSignature: repeat.upperTimeSignature
        lowerTimeSignature: repeat.lowerTimeSignature
        currentFormat: repeat.timeFormat
        // Keep last: setting the value is what refreshes `valueString`
        value: repeat.selectionDuration
    }

    TimecodeModel {
        id: newLengthFormatter

        mode: TimecodeModeSelector.Duration
        sampleRate: repeat.sampleRate
        tempo: repeat.tempo
        upperTimeSignature: repeat.upperTimeSignature
        lowerTimeSignature: repeat.lowerTimeSignature
        currentFormat: repeat.timeFormat
        // Keep last: setting the value is what refreshes `valueString`
        value: repeat.newSelectionDuration
    }

    ColumnLayout {
        id: column

        width: parent.width
        spacing: 16

        RowLayout {
            Layout.fillWidth: true
            // End the field where the preset selector above ends: before the presets bar's 4 buttons and their spacing
            Layout.rightMargin: 4 * (ui.theme.defaultButtonSize + 4)
            spacing: 12

            StyledTextLabel {
                Layout.fillWidth: true
                horizontalAlignment: Text.AlignLeft
                text: qsTrc("effects/repeat", "Number of repeats to add")
            }

            TextMetrics {
                id: maxCountMetrics
                font: ui.theme.bodyFont
                text: "000"
            }

            IncrementalPropertyControl {
                id: repeatCountControl

                // Just room for 3 digits and the arrows: text padding + digits + gap + arrows with border
                Layout.preferredWidth: 12 + Math.ceil(maxCountMetrics.advanceWidth) + 4 + 23

                navigation.panel: root.repeatNavigationPanel
                navigation.order: 0
                navigation.accessible.name: qsTrc("effects/repeat", "Number of repeats to add")

                currentValue: repeat.repeatCount
                minValue: repeat.repeatCountMin
                maxValue: repeat.repeatCountMax
                decimals: 0
                step: 1

                // Reject numbers above the maximum while typing (the default validator lets them through
                // and only clamps them on commit, so the field could show a count other than the one applied).
                // 1 to 100: matches RepeatEffect::MaxRepeatCount.
                // Empty is accepted too, so that clearing the field is reported and disables Apply.
                validator: RegularExpressionValidator {
                    regularExpression: /^(100|[1-9]\d?)?$/
                }

                onValueEdited: function (newValue) {
                    // An empty field comes as 0 (the validator rejects a typed 0): keep the count but don't apply
                    root.isApplyAllowed = newValue >= repeat.repeatCountMin
                    if (root.isApplyAllowed) {
                        repeat.repeatCount = newValue
                    }
                }

                onAccepted: {
                    root.applyRequested()
                }
            }
        }

        RoundedRectangle {
            Layout.fillWidth: true
            implicitWidth: summary.implicitWidth + 2 * summary.anchors.margins
            implicitHeight: summary.implicitHeight + 2 * summary.anchors.margins

            color: ui.theme.backgroundSecondaryColor
            radius: 4
            border.color: ui.theme.strokeColor
            border.width: 1

            ColumnLayout {
                id: summary

                anchors.fill: parent
                anchors.margins: 12
                spacing: 10

                RowLayout {
                    Layout.fillWidth: true
                    spacing: 16

                    StyledTextLabel {
                        Layout.fillWidth: true
                        horizontalAlignment: Text.AlignLeft
                        text: qsTrc("effects/repeat", "Current length")
                    }

                    StyledTextLabel {
                        horizontalAlignment: Text.AlignRight
                        text: root.spaced(currentLengthFormatter.valueString)
                    }
                }

                SeparatorLine {
                    Layout.fillWidth: true
                }

                RowLayout {
                    Layout.fillWidth: true
                    spacing: 16

                    StyledTextLabel {
                        Layout.fillWidth: true
                        horizontalAlignment: Text.AlignLeft
                        font: ui.theme.bodyBoldFont
                        text: qsTrc("effects/repeat", "New length")
                    }

                    StyledTextLabel {
                        horizontalAlignment: Text.AlignRight
                        font: ui.theme.bodyBoldFont
                        text: root.spaced(newLengthFormatter.valueString)
                    }
                }
            }
        }
    }
}
