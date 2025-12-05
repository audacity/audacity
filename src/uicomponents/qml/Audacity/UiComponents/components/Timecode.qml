/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Audacity.UiComponents 1.0

import "internal"

NumericView {
    id: root

    property alias value: timecodeModel.value
    property alias mode: timecodeModel.mode

    property alias sampleRate: timecodeModel.sampleRate
    property alias tempo: timecodeModel.tempo
    property alias upperTimeSignature: timecodeModel.upperTimeSignature
    property alias lowerTimeSignature: timecodeModel.lowerTimeSignature

    property alias currentFormat: timecodeModel.currentFormat
    property alias currentFormatStr: timecodeModel.currentFormatStr

    model: timecodeModel

    TimecodeModel {
        id: timecodeModel

        onValueChanged: {
            root.valueChangeRequested(value)
        }
    }
}
