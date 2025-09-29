import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Playback
import Audacity.Effects
import Audacity.BuiltinEffects

BuiltinEffectBase {
    id: root

    property string title: qsTrc("projectscene/silence", "Silence")
    property alias isApplyAllowed: silence.isApplyAllowed

    implicitWidth: 300
    implicitHeight: column.implicitHeight

    model: silence

    SilenceViewModel {
        id: silence
    }

    Column {
        id: column

        anchors.fill: parent

        spacing: 16

        StyledTextLabel {
            text: qsTrc("projectscene/silence", "Duration:")
        }

        Timecode {
            id: timecode

            value: silence.duration
            mode: TimecodeModeSelector.Duration
            currentFormatStr: silence.durationFormat
            sampleRate: silence.sampleRate
            tempo: silence.tempo
            upperTimeSignature: silence.upperTimeSignature
            lowerTimeSignature: silence.lowerTimeSignature

            onValueChanged: {
                silence.duration = timecode.value
            }

            onCurrentFormatChanged: {
                silence.durationFormat = timecode.currentFormatStr
            }
        }
    }
}
