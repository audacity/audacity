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

        RealInputField {
            Layout.preferredWidth: 80

            min: 1
            max: 1000000

            currentValue: tone.frequencyStart
            onCurrentValueChanged: {
                tone.frequencyStart = currentValue
            }
        }

        // Third row
        StyledTextLabel {
            text: qsTrc("effects/tone", "Amplitude (0-1):")
        }

        RealInputField {
            Layout.preferredWidth: 80

            min: 0
            max: 1

            currentValue: tone.amplitudeStart
            onCurrentValueChanged: {
                tone.amplitudeStart = currentValue
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
            mode: TimecodeModeSelector.Duration
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
