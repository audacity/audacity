import QtQuick
import QtQuick.Controls
import Muse.UiComponents
import Audacity.Effects
import Audacity.Playback
import "../common"

EffectBase {

    property string title: "Tone" // review
    property alias instanceId: tone.instanceId

    property bool isApplyAllowed: true

    width: 300
    height: 200

    ToneViewModel {
        id: tone
        onSampleRateChanged: {
            timecode.sampleRate = tone.sampleRate
        }
    }

    Component.onCompleted: {
        tone.init()
    }

    Column {
        anchors.fill: parent
        spacing: 16

        Row {

            anchors.horizontalCenter: parent.horizontalCenter

            spacing: 4

            StyledTextLabel {
                anchors.verticalCenter: parent.verticalCenter
                text: qsTrc("effects/tone", "Waveform:")
            }

            ComboBox {
                anchors.verticalCenter: parent.verticalCenter
                width: 80

                model: ["Sine", "Square", "Sawtooth", "Square, no alias", "Triangle"]
                currentIndex: tone.waveform

                onActivated: function (index) {
                    tone.waveform = index
                }
            }
        }

        Row {
            anchors.horizontalCenter: parent.horizontalCenter

            spacing: 4

            StyledTextLabel {
                anchors.verticalCenter: parent.verticalCenter

                text: qsTrc("effects/tone", "Frequency (Hz):")
            }

            TextInputField {
                anchors.verticalCenter: parent.verticalCenter
                width: 80

                currentText: tone.frequency.toFixed(4)

                onTextEdited: function (newTextValue) {
                    tone.frequency = parseFloat(newTextValue)
                }
            }
        }

        Row {
            anchors.horizontalCenter: parent.horizontalCenter

            spacing: 4

            StyledTextLabel {
                anchors.verticalCenter: parent.verticalCenter

                text: qsTrc("effects/tone", "Amplitude (0-1):")
            }

            TextInputField {
                anchors.verticalCenter: parent.verticalCenter
                width: 80

                currentText: tone.amplitude.toFixed(4)

                validator: DoubleInputValidator {
                    top: 1
                    bottom: 0
                    decimal: 4
                }

                onTextEdited: function (newTextValue) {
                    tone.amplitude = parseFloat(newTextValue)
                }
            }
        }

        Row {
            spacing: 4

            anchors.horizontalCenter: parent.horizontalCenter

            Timecode {
               id: timecode
               value: tone.duration
               currentFormatStr: tone.durationFormat
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
}
