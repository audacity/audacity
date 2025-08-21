import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects
import Audacity.Playback
import "../common"

// TODO: move to common controls
import Preferences

BuiltinEffectBase {
    id: root

    property string title: qsTrc("effects/tone", "Tone")
    property alias isApplyAllowed: tone.isApplyAllowed

    width: 360
    implicitHeight: column.height

    model: tone

    QtObject {
        id: prv

        readonly property int spacing: 16
        readonly property int interpolationLinear: 0
        readonly property int interpolationLogarithmic: 1
    }

    ToneViewModel {
        id: tone
    }

    Component.onCompleted: {
        tone.init()
    }

    Column {
        id: column

        width: parent.width

        spacing: prv.spacing

        ComboBoxWithTitle {

            title: qsTrc("effects/tone", "Waveform")
            columnWidth: parent.width

            control.background.color: ui.theme.backgroundPrimaryColor
            control.background.border.width: 1
            control.itemColor: "transparent"

            currentIndex: tone.waveform
            model: tone.waveforms

            onValueEdited: function (newIndex, newValue) {
                tone.waveform = newIndex
            }
        }

        IncrementalPropertyControlWithTitle {

            columnWidth: (parent.width - parent.spacing) / 2
            controlWidth: (parent.width - parent.spacing) / 2

            title: qsTrc("effects/tone", "Frequency")
            currentValue: tone.frequencyStart

            minValue: 1
            maxValue: 1000000

            measureUnitsSymbol: qsTrc("global", "Hz")

            onValueEdited: function (newValue) {
                if (tone.frequencyStart !== newValue) {
                    tone.frequencyStart = newValue
                }
            }
        }

        IncrementalPropertyControlWithTitle {

            columnWidth: (parent.width - parent.spacing) / 2
            controlWidth: parent.width * .25

            title: qsTrc("effects/tone", "Amplitude (0-1)")
            currentValue: tone.amplitudeStart

            minValue: 0
            maxValue: 1
            decimals: 4
            step: 0.01

            onValueEdited: function (newValue) {
                if (tone.amplitudeStart !== newValue) {
                    tone.amplitudeStart = newValue
                }
            }
        }

        Column {

            spacing: 8

            StyledTextLabel {
                text: qsTrc("effects/tone", "Duration")
            }

            Timecode {
                id: timecode

                Layout.fillHeight: false
                Layout.columnSpan: 2

                border: Border {
                    color: ui.theme.strokeColor
                    width: 1
                }

                arrowSpacing: -2
                backgroundColor: ui.theme.backgroundSecondaryColor
                textColor: ui.theme.fontPrimaryColor

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
}
