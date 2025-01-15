import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects
import Audacity.Playback
import "../common"

EffectBase {

    id: root

    property string title: qsTrc("effects", "Noise")
    property alias isApplyAllowed: noise.isApplyAllowed

    width: 300
    height: 150

    model: noise

    NoiseViewModel {
        id: noise
        instanceId: root.instanceId
    }

    Component.onCompleted: {
        noise.init()
    }

    GridLayout {
        columns: 2
        rows: 3

        columnSpacing: 10
        rowSpacing: 16

        anchors.fill: parent
        anchors.margins: 10

        // First row
        StyledTextLabel {
            text: qsTrc("effects/noise", "Type:")
        }

        ComboBox {
            id: typeSelector

            Layout.fillWidth: true

            model: noise.types
            textRole: "text"
            valueRole: "value"
            currentIndex: noise.types.findIndex(type => type.value === noise.type)

            onActivated: function (index) {
                noise.type = currentValue
            }
        }

        // Second row
        StyledTextLabel {
            text: qsTrc("effects/noise", "Duration:")
        }

        Timecode {
            id: timecode

            Layout.fillWidth: true
            Layout.fillHeight: false

            value: noise.duration
            mode: TimecodeModeSelector.Duration
            currentFormatStr: noise.durationFormat
            sampleRate: noise.sampleRate
            tempo: noise.tempo
            upperTimeSignature: noise.upperTimeSignature
            lowerTimeSignature: noise.lowerTimeSignature

            onValueChanged: {
                noise.duration = timecode.value
            }
        }

        // Third row
        StyledTextLabel {
            text: qsTrc("effects/noise", "Amplitude (0-1):")
        }

        RealInputField {
            Layout.fillWidth: true

            min: 0
            max: 1

            currentValue: noise.amplitude
            onCurrentValueChanged: {
                noise.amplitude = currentValue
            }
        }
    }
}
