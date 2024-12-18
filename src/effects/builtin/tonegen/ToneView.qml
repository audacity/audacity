import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects
import Audacity.Playback
import "../common"

EffectBase {

    id: root

    property string title: qsTrc("effects", "Tone")
    property alias isApplyAllowed: tone.isApplyAllowed

    width: 370
    height: 200

    model: tone

    ToneViewModel {
        id: tone
        instanceId: root.instanceId
    }

    Component.onCompleted: {
        tone.init()
    }

    GridLayout {
        columns: 2
        rows: 4
        columnSpacing: 4
        rowSpacing: 16

        anchors.horizontalCenter: parent.horizontalCenter
        anchors.fill: parent
        anchors.margins: 10

        // First row
        StyledTextLabel {
            text: qsTrc("effects/tone", "Waveform:")
        }

        ComboBox {
            Layout.preferredWidth: 80

            model: tone.waveforms
            currentIndex: tone.waveform

            onActivated: function (index) {
                tone.waveform = index
            }
        }

        // Second row
        StyledTextLabel {
            text: qsTrc("effects", "Frequency (Hz):")
        }

        TextInputField {
            Layout.preferredWidth: 80

            currentText: tone.frequencyStart

            validator: DoubleValidator {
                bottom: 1
            }

            onTextEdited: function (newTextValue) {
                tone.frequencyStart = parseFloat(newTextValue)
            }
        }

        // Third row
        StyledTextLabel {
            text: qsTrc("effects/tone", "Amplitude (0-1):")
        }

        TextInputField {
            Layout.preferredWidth: 80

            currentText: tone.amplitudeStart

            validator: DoubleValidator {
                top: 1
                bottom: 0
            }

            onTextEdited: function (newTextValue) {
                tone.amplitudeStart = parseFloat(newTextValue)
            }
        }

        // Fourth row
        StyledTextLabel {
            text: qsTrc("effects/tone", "Duration:")
        }

        Timecode {
            Layout.fillHeight: false

            id: timecode

            value: tone.duration
            currentFormatStr: tone.durationFormat
            sampleRate: tone.sampleRate
            tempo: tone.tempo
            upperTimeSignature: tone.upperTimeSignature
            lowerTimeSignature: tone.lowerTimeSignature
            enabled: true

            onValueChanged: {
                tone.duration = timecode.value
            }
        }
    }
}
