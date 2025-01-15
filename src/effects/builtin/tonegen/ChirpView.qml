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

        RealInputField {
            Layout.preferredWidth: 80

            min: 1
            max: 1000000

            currentValue: chirp.frequencyStart
            onCurrentValueChanged: {
                chirp.frequencyStart = currentValue
            }
        }

        RealInputField {
            Layout.preferredWidth: 80

            min: 1
            max: 1000000

            currentValue: chirp.frequencyEnd
            onCurrentValueChanged: {
                chirp.frequencyEnd = currentValue
            }
        }

        // Third row
        StyledTextLabel {
            text: qsTrc("effects/chirp", "Amplitude (0-1):")
        }

        RealInputField {
            Layout.preferredWidth: 80

            min: 0
            max: 1

            currentValue: chirp.amplitudeStart
            onCurrentValueChanged: {
                chirp.amplitudeStart = currentValue
            }
        }

        RealInputField {
            Layout.preferredWidth: 80

            min: 0
            max: 1

            currentValue: chirp.amplitudeEnd
            onCurrentValueChanged: {
                chirp.amplitudeEnd = currentValue
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
            mode: TimecodeModeSelector.Duration
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
