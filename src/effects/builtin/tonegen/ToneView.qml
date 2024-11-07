import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects
import Audacity.Playback
import "../common"

EffectBase {

    // Helper function to format number
    function formatNumber(value, maxDecimals) {
        // Convert number to fixed decimal string and remove trailing zeros
        return parseFloat(value.toFixed(maxDecimals)).toString();
    }

    property string title: qsTrc("effects", "Tone")
    property alias instanceId: tone.instanceId
    property alias isApplyAllowed: tone.isApplyAllowed

    width: 370
    height: 200

    function preview() {
        tone.preview()
    }

    ToneViewModel {
        id: tone
    }

    Component.onCompleted: {
        tone.init()
        timecode.currentFormatStr = tone.durationFormat
    }

    GridLayout {
        columns: 2
        rows: 4
        columnSpacing: 4
        rowSpacing: 16

        // Ensure the items are left-aligned in their respective cells (suggested by ChatGPT, review this)
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.fill: parent
        anchors.margins: 10

        // First row
        StyledTextLabel {
            text: qsTrc("effects/tone", "Waveform:")
        }

        ComboBox {
            width: 80
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
            width: 80
            currentText: formatNumber(tone.frequency, 6)

            validator: DoubleInputValidator {
                decimal: 6
            }

            onTextEdited: function (newTextValue) {
                tone.frequency = parseFloat(newTextValue)
            }
        }

        // Third row
        StyledTextLabel {
            text: qsTrc("effects/tone", "Amplitude (0-1):")
        }

        TextInputField {
            width: 80
            currentText: formatNumber(tone.amplitude, 6)

            validator: DoubleInputValidator {
                decimal: 6
            }

            onTextEdited: function (newTextValue) {
                tone.amplitude = parseFloat(newTextValue)
            }
        }

        // Fourth row
        StyledTextLabel {
            text: qsTrc("effects", "Duration:")
        }

        Timecode {
            Layout.fillHeight: false
            id: timecode
            value: tone.duration
            currentFormatStr: tone.durationFormat
            sampleRate: tone.sampleRate
            enabled: true

            onValueChanged: {
                tone.duration = timecode.value
            }

            onCurrentFormatChanged: {
                tone.durationFormat = timecode.currentFormatStr
            }
        }
    }
}
