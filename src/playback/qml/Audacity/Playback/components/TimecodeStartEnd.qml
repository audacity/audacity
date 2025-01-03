/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.Playback 1.0

RowLayout {
    id: root

    property alias startValue: startTimecode.value
    property alias endValue: endTimecode.value

    property real sampleRate: 0.0
    property real tempo: 0.0
    property int upperTimeSignature: 0
    property int lowerTimeSignature: 0

    property int currentFormat: 0

    signal startValueChangeRequested(var newValue)
    signal endValueChangeRequested(var newValue)
    signal formatChangeRequested(var newFormat)

    spacing: 1

    Timecode {
        id: startTimecode

        mode: TimecodeModeSelector.TimePoint
        sampleRate: root.sampleRate
        tempo: root.tempo
        upperTimeSignature: root.upperTimeSignature
        lowerTimeSignature: root.lowerTimeSignature

        currentFormat: root.currentFormat

        showMenu: false

        onValueChangeRequested: function(newValue) {
            root.startValueChangeRequested(newValue)
        }
    }

    Timecode {
        id: endTimecode

        mode: TimecodeModeSelector.TimePoint
        sampleRate: root.sampleRate
        tempo: root.tempo
        upperTimeSignature: root.upperTimeSignature
        lowerTimeSignature: root.lowerTimeSignature

        currentFormat: root.currentFormat

        backgroundLeftRadius: 0

        onValueChangeRequested: function(newValue) {
            root.endValueChangeRequested(newValue)
        }

        onCurrentFormatChanged: function() {
            root.formatChangeRequested(currentFormat)
        }
    }
}
