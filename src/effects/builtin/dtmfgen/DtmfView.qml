import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects
import Audacity.Playback
import "../common"

EffectBase {

    id: root

    property string title: qsTrc("effects/dtmf", "DTMF Tones")
    property alias isApplyAllowed: dtmf.isApplyAllowed

    width: 300
    height: 350

    model: dtmf

    DtmfViewModel {
        id: dtmf
        instanceId: root.instanceId
    }

    Component.onCompleted: {
        dtmf.init()
    }

    GridLayout {
        columns: 2
        rows: 3

        columnSpacing: 10
        rowSpacing: 16

        anchors.fill: parent
        anchors.margins: 10

        StyledTextLabel {
            text: qsTrc("effects/dtmf", "DTMF sequence:")
        }

        TextInputField {
            Layout.fillWidth: true

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
            text: qsTrc("effects/dtmf", "Amplitude (0-1):")
        }

        RealInputField {
            Layout.fillWidth: true

            min: 0
            max: 1

            currentValue: dtmf.amplitude
            onCurrentValueChanged: {
                dtmf.amplitude = currentValue
            }
        }

        StyledTextLabel {
            text: qsTrc("effects/dtmf", "Duration:")
        }

        Timecode {
            id: timecode

            Layout.fillWidth: true
            Layout.fillHeight: false

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

        StyledTextLabel {
            text: qsTrc("effects/dtmf", "Tone/silence ratio:")
        }

        StyledSlider {
            from: 0
            to: 100
            stepSize: 0.1
            value: dtmf.dutyCycle
            onMoved: dtmf.dutyCycle = value
        }

        StyledTextLabel {
            text: qsTrc("effects/dtmf", "Duty cycle:")
        }
        StyledTextLabel {
            text: qsTrc("effects/dtmf", "%1%").arg(dtmf.dutyCycle.toFixed(1))
        }

        StyledTextLabel {
            text: qsTrc("effects/dtmf", "Tone duration:")
        }
        StyledTextLabel {
            text: qsTrc("effects/dtmf", "%1 ms").arg(Math.round(dtmf.toneDuration * 1000))
        }

        StyledTextLabel {
            text: qsTrc("effects/dtmf", "Silence duration:")
        }
        StyledTextLabel {
            text: qsTrc("effects/dtmf", "%1 ms").arg(Math.round(dtmf.silenceDuration * 1000))
        }
    }
}

