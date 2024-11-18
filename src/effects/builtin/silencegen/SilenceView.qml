import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects
import Audacity.Playback
import "../common"

EffectBase {
    property string title: qsTrc("effects", "Silence")
    property alias instanceId: silence.instanceId
    property bool isApplyAllowed: true

    height: 100

    function preview() {
        silence.preview()
    }

    SilenceViewModel {
        id: silence
    }

    Component.onCompleted: {
        silence.init()
        timecode.currentFormatStr = silence.durationFormat
    }

    RowLayout {
        anchors.fill: parent
        anchors.margins: 20

        StyledTextLabel {
            text: qsTrc("effects", "Duration:")
        }

        Timecode {
            id: timecode

            Layout.fillHeight: false

            value: silence.duration
            currentFormatStr: silence.durationFormat
            sampleRate: silence.sampleRate

            onValueChanged: {
                silence.duration = timecode.value
            }

            onCurrentFormatChanged: {
                silence.durationFormat = timecode.currentFormatStr
            }
        }
    }
}
