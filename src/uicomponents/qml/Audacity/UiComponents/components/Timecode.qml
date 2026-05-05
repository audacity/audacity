/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.UiComponents 1.0

import "internal"

NumericView {
    id: root

    enum Appearance {
        Themed,
        Clock
    }

    property int appearance: Timecode.Appearance.Themed

    property alias value: timecodeModel.value
    property alias mode: timecodeModel.mode

    property alias sampleRate: timecodeModel.sampleRate
    property alias tempo: timecodeModel.tempo
    property alias upperTimeSignature: timecodeModel.upperTimeSignature
    property alias lowerTimeSignature: timecodeModel.lowerTimeSignature

    property alias currentFormat: timecodeModel.currentFormat
    property alias currentFormatStr: timecodeModel.currentFormatStr

    backgroundColor: appearance === Timecode.Appearance.Clock ? ui.theme.backgroundQuarternaryColor : ui.theme.backgroundSecondaryColor
    textColor: appearance === Timecode.Appearance.Clock ? ui.theme.fontSecondaryColor : ui.theme.fontPrimaryColor
    border: Border {
        color: ui.theme.strokeColor
        width: 1
    }
    arrowSpacing: -2

    model: timecodeModel

    TimecodeModel {
        id: timecodeModel

        onValueChanged: {
            root.valueChangeRequested(value)
        }
    }
}
