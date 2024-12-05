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
    property bool isApplyAllowed: true

    width: 300
    height: 150

    model: noise

    NoiseViewModel {
        id: noise
        instanceId: root.instanceId
    }

    Component.onCompleted: {
        noise.init()
        timecode.currentFormatStr = noise.durationFormat
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
            currentFormatStr: noise.durationFormat
            sampleRate: noise.sampleRate

            onValueChanged: {
                noise.duration = timecode.value
            }

            onCurrentFormatChanged: {
                noise.durationFormat = timecode.currentFormatStr
            }
        }

        // Third row
        StyledTextLabel {
            text: qsTrc("effects/noise", "Amplitude (0-1):")
        }

        TextInputField {
            Layout.fillWidth: true

            currentText: noise.amplitude

            validator: DoubleInputValidator {
                decimal: 6
            }

            onTextEdited: function (newTextValue) {
                noise.amplitude = parseFloat(newTextValue)
            }
        }
    }
}
