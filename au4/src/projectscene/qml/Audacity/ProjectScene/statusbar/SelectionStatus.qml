import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene
import Audacity.Playback

Row {
    spacing: 6

    SelectionStatusModel {
        id: selectionModel
    }

    Component.onCompleted: {
        selectionModel.init()
    }

    StyledTextLabel {
        anchors.verticalCenter: parent.verticalCenter

        text: qsTrc("projectscene", "Selection")

        enabled: selectionModel.isEnabled
        opacity: enabled ? 1.0 : ui.theme.itemOpacityDisabled
    }

    TimecodeStartEnd {
        startValue: selectionModel.startTime
        endValue: selectionModel.endTime

        sampleRate: selectionModel.sampleRate
        tempo: selectionModel.tempo
        upperTimeSignature: selectionModel.upperTimeSignature
        lowerTimeSignature: selectionModel.lowerTimeSignature

        currentFormat: selectionModel.currentFormat

        enabled: selectionModel.isEnabled

        onStartValueChangeRequested: function(newValue) {
            selectionModel.startTime = newValue
        }

        onEndValueChangeRequested: function(newValue) {
            selectionModel.endTime = newValue
        }

        onFormatChangeRequested: function(newFormat) {
            selectionModel.currentFormat = newFormat
        }
    }
}
