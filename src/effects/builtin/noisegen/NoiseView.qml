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

    property string title: qsTrc("effects", "Noise")
    property alias isApplyAllowed: noise.isApplyAllowed

    width: 300
    implicitHeight: column.height

    model: noise

    NoiseViewModel {
        id: noise
    }

    Component.onCompleted: {
        noise.init()
    }

    Column {
        id: column

        width: parent.width
        spacing: 16

        ComboBoxWithTitle {
            id: typeSelector

            columnWidth: parent.width
            title: qsTrc("effects/noise", "Type")
            model: noise.types
            textRole: "text"
            valueRole: "value"
            currentIndex: noise.types.findIndex(type => type.value === noise.type)

            control.background.color: ui.theme.backgroundPrimaryColor
            control.background.border.width: 1
            control.itemColor: "transparent"

            onValueEdited: function (newIndex, newValue) {
                noise.type = newIndex
            }
        }

        IncrementalPropertyControlWithTitle {

            title: qsTrc("effects/noise", "Amplitude (0-1)")

            minValue: 0
            maxValue: 1
            decimals: 4
            step: 0.01
            currentValue: noise.amplitude

            onValueEdited: function (newValue) {
                if (noise.amplitude !== newValue) {
                    noise.amplitude = newValue
                }
            }
        }

        Column {

            spacing: 8

            StyledTextLabel {
                text: qsTrc("effects/noise", "Duration")
            }

            Timecode {
                id: timecode

                Layout.fillWidth: true
                Layout.fillHeight: false

                border: Border {
                    color: ui.theme.strokeColor
                    width: 1
                }

                arrowSpacing: -2
                backgroundColor: ui.theme.backgroundSecondaryColor
                textColor: ui.theme.fontPrimaryColor
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
        }
    }
}
