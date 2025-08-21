import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Playback
import Audacity.Effects
import "../common"

BuiltinEffectBase {
    id: root

    property string title: qsTrc("projectscene/silence", "Silence")
    property alias isApplyAllowed: silence.isApplyAllowed

    implicitHeight: 60
    model: silence

    SilenceViewModel {
        id: silence
    }

    Component.onCompleted: {
        silence.init()
    }

    Row {
        id: row

        anchors.fill: parent
        anchors.margins: 0
        spacing: 16

        StyledTextLabel {
            text: qsTrc("projectscene/silence", "Duration:")
            anchors.verticalCenter: parent.verticalCenter
        }

        Timecode {
            id: timecode

            anchors.verticalCenter: parent.verticalCenter

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
