/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Audacity.UiComponents 1.0

import "internal"

NumericView {
    id: root

    property alias value: frequencyModel.value

    property alias sampleRate: frequencyModel.sampleRate
    property alias tempo: frequencyModel.tempo
    property alias upperTimeSignature: frequencyModel.upperTimeSignature
    property alias lowerTimeSignature: frequencyModel.lowerTimeSignature

    property alias currentFormat: frequencyModel.currentFormat
    property alias currentFormatStr: frequencyModel.currentFormatStr

    model: frequencyModel

    FrequencyModel {
        id: frequencyModel

        onValueChanged: {
            root.valueChangeRequested(value)
        }
    }
}
