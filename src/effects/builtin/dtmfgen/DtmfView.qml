import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects
import Audacity.Playback
import Audacity.ProjectScene
import "../common"

// TODO: move to common controls
import Preferences

BuiltinEffectBase {
    id: root

    property alias instanceId: dtmf.instanceId

    property string title: qsTrc("effects/dtmf", "DTMF tones")
    property alias isApplyAllowed: dtmf.isApplyAllowed

    width: 550
    implicitHeight: row.height

    model: dtmf

    QtObject {
        id: prv

        readonly property int spacing: 16
    }

    DtmfViewModel {
        id: dtmf
    }

    Component.onCompleted: {
        dtmf.init()
    }

    RowLayout {
        id: row

        width: parent.width
        spacing: prv.spacing

        ColumnLayout {
            id: leftColumn

            Layout.fillWidth: true
            Layout.preferredWidth: 1

            spacing: prv.spacing

            StyledTextLabel {
                text: qsTrc("effects/dtmf", "DTMF sequence")
            }

            TextInputField {

                currentText: dtmf.sequence

                validator: RegularExpressionValidator {
                    regularExpression: /[0-9A-Da-z\*\#]*/
                }

                onTextCleared: {
                    dtmf.sequence = ""
                }

                onTextEdited: function (newTextValue) {
                    dtmf.sequence = newTextValue
                }
            }

            StyledTextLabel {

                Layout.fillWidth: true
                Layout.alignment: Qt.AlignTop

                verticalAlignment: Text.AlignTop
                horizontalAlignment: Text.AlignLeft

                text: qsTrc("effects/dtmf", "For each tone you wish to generate, enter numbers from 0 to 9, lower case letters from a to z, and the * and # characters.")

                wrapMode: Text.WordWrap
                font.italic: true
            }

            IncrementalPropertyControlWithTitle {

                title: qsTrc("effects/dtmf", "Amplitude (0-1)")
                currentValue: dtmf.amplitude

                minValue: 0
                maxValue: 1
                decimals: 4
                step: 0.01

                onValueEdited: function (newValue) {
                    if (dtmf.amplitude !== newValue) {
                        dtmf.amplitude = newValue
                    }
                }
            }

            Column {

                spacing: 8

                StyledTextLabel {
                    text: qsTrc("effects/dtmf", "Duration")
                }

                Timecode {
                    id: timecode

                    Layout.fillWidth: true
                    Layout.fillHeight: false

                    border: Border {
                        color: ui.theme.strokeColor
                        width: 1
                    }

                    arrowSpacing: -2
                    backgroundColor: ui.theme.backgroundSecondaryColor
                    textColor: ui.theme.fontPrimaryColor

                    value: dtmf.duration
                    mode: TimecodeModeSelector.Duration
                    currentFormatStr: dtmf.durationFormat
                    sampleRate: dtmf.sampleRate
                    tempo: dtmf.tempo
                    upperTimeSignature: dtmf.upperTimeSignature
                    lowerTimeSignature: dtmf.lowerTimeSignature

                    onValueChanged: {
                        dtmf.duration = timecode.value
                    }
                }
            }

            Item {
                Layout.preferredHeight: 1
            }
        }

        RoundedRectangle {

            Layout.fillWidth: true
            Layout.preferredWidth: 1
            Layout.preferredHeight: leftColumn.height

            color: ui.theme.backgroundSecondaryColor

            border.color: ui.theme.strokeColor
            border.width: 1

            radius: 4

            ColumnLayout {

                x: prv.spacing
                y: prv.spacing

                spacing: prv.spacing

                StyledTextLabel {
                    text: qsTrc("effects/dtmf", "Tone/silence ratio")
                }

                Row {

                    spacing: 8

                    KnobControl {

                        from: 0
                        to: 100
                        stepSize: 0.1
                        value: dtmf.dutyCycle

                        onNewValueRequested: function (value) {
                            let newValue = +(value.toFixed(1))
                            if (newValue !== dtmf.dutyCycle) {
                                dtmf.dutyCycle = newValue
                            }
                        }
                    }

                    IncrementalPropertyControl {

                        implicitWidth: 60

                        step: 1
                        decimals: 0
                        minValue: 0
                        maxValue: 1000
                        currentValue: dtmf.dutyCycle * 10

                        onValueEdited: function (newValue) {
                            newValue = newValue / 10
                            if (newValue !== dtmf.dutyCycle) {
                                dtmf.dutyCycle = newValue
                            }
                        }
                    }
                }

                Column {

                    spacing: 8

                    StyledTextLabel {
                        text: qsTrc("effects/dtmf", "Duty cycle")
                    }

                    StyledTextLabel {
                        text: qsTrc("effects/dtmf", "%1%").arg(dtmf.dutyCycle.toFixed(1))
                        font.bold: true
                    }
                }

                Column {

                    spacing: 8

                    StyledTextLabel {
                        text: qsTrc("effects/dtmf", "Tone duration")
                    }

                    StyledTextLabel {
                        text: qsTrc("effects/dtmf", "%1 ms").arg(Math.round(dtmf.toneDuration * 1000))
                        font.bold: true
                    }
                }

                Column {

                    spacing: 8

                    StyledTextLabel {
                        text: qsTrc("effects/dtmf", "Silence duration")
                    }

                    StyledTextLabel {
                        text: qsTrc("effects/dtmf", "%1 ms").arg(Math.round(dtmf.silenceDuration * 1000))
                        font.bold: true
                    }
                }
            }
        }
    }
}
