import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects
import Audacity.Playback
import "../common"

EffectBase {

    id: root

    property string title: qsTrc("effects", "Chirp")
    property alias isApplyAllowed: chirp.isApplyAllowed

    width: 370
    height: 230

    model: chirp

    ToneViewModel {
        id: chirp
        instanceId: root.instanceId
    }

    Component.onCompleted: {
        chirp.init()
    }

    GridLayout {
        columns: 3
        rows: 5
        columnSpacing: 4
        rowSpacing: 16

        anchors.horizontalCenter: parent.horizontalCenter
        anchors.fill: parent
        anchors.margins: 10

        // First row
        StyledTextLabel {
            text: qsTrc("effects/chirp", "Waveform:")
        }

        ComboBox {
            Layout.columnSpan: 2

            model: chirp.waveforms
            currentIndex: chirp.waveform

            onActivated: function (index) {
                chirp.waveform = index
            }
        }

        // Second row
        StyledTextLabel {
            text: qsTrc("effects/chirp", "Frequency (Hz):")
        }

        TextInputField {
            Layout.preferredWidth: 80

            currentText: chirp.frequencyStart

            validator: DoubleValidator {
                bottom: 1
            }

            inputField.onDisplayTextChanged: function () {
                chirp.frequencyStart = parseFloat(inputField.displayText)
            }
        }

        TextInputField {
            Layout.preferredWidth: 80
            currentText: chirp.frequencyEnd

            validator: DoubleValidator {
                bottom: 1
            }

            inputField.onDisplayTextChanged: function () {
                chirp.frequencyEnd = parseFloat(inputField.displayText)
            }
        }

        // Third row
        StyledTextLabel {
            text: qsTrc("effects/chirp", "Amplitude (0-1):")
        }

        TextInputField {
            Layout.preferredWidth: 80

            currentText: chirp.amplitudeStart

            validator: DoubleValidator {
                top: 1
                bottom: 0
            }

            inputField.onDisplayTextChanged: function () {
                chirp.amplitudeStart = parseFloat(inputField.displayText)
            }
        }

        TextInputField {
            Layout.preferredWidth: 80

            currentText: chirp.amplitudeEnd

            validator: DoubleValidator {
                top: 1
                bottom: 0
            }

            inputField.onDisplayTextChanged: function () {
                chirp.amplitudeEnd = parseFloat(inputField.displayText)
            }
        }

        // Fourth row        
        StyledTextLabel {
            text: qsTrc("effects/chirp", "Interpolation:")
        }
        ComboBox {
            Layout.columnSpan: 2

            model: chirp.interpolationTypes
            currentIndex: chirp.interpolation

            onActivated: function (index) {
                chirp.interpolation = index
            }
        }
        
        // Fifth row
        StyledTextLabel {
            text: qsTrc("effects/chirp", "Duration:")
        }

        Timecode {
            Layout.fillHeight: false
            Layout.columnSpan: 2

            id: timecode

            value: chirp.duration
            currentFormatStr: chirp.durationFormat
            sampleRate: chirp.sampleRate
            tempo: chirp.tempo
            upperTimeSignature: chirp.upperTimeSignature
            lowerTimeSignature: chirp.lowerTimeSignature
            enabled: true

            onValueChanged: {
                chirp.duration = timecode.value
            }
        }
    }
}
