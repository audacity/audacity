import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene
import Audacity.UiComponents 1.0

Row {
    id: root

    property NavigationPanel navigationPanel: null

    spacing: 6

    SelectionStatusModel {
        id: selectionModel
    }

    Component.onCompleted: {
        selectionModel.init()
    }

    StyledTextLabel {
        id: titleLabel

        anchors.verticalCenter: parent.verticalCenter

        text: qsTrc("projectscene", "Selection")

        enabled: selectionModel.isEnabled
        opacity: enabled ? 1.0 : ui.theme.itemOpacityDisabled
    }

    TimecodeStartEnd {
        id: startEndTimeCode

        startValue: selectionModel.startTime
        endValue: selectionModel.endTime

        sampleRate: selectionModel.sampleRate
        tempo: selectionModel.tempo
        upperTimeSignature: selectionModel.upperTimeSignature
        lowerTimeSignature: selectionModel.lowerTimeSignature

        currentFormat: selectionModel.currentFormat

        enabled: selectionModel.isEnabled

        navigationPanel: root.navigationPanel
        startAccessibleName: qsTrc("projectscene", "Selection start")
        endAccessibleName: qsTrc("projectscene", "Selection end")

        onStartValueChangeRequested: function (newValue) {
            selectionModel.startTime = newValue
        }

        onEndValueChangeRequested: function (newValue) {
            selectionModel.endTime = newValue
        }

        onFormatChangeRequested: function (newFormat) {
            selectionModel.currentFormat = newFormat
        }
    }

    StyledTextLabel {
        id: durationLabel

        anchors.verticalCenter: parent.verticalCenter

        text: qsTrc("projectscene", "Duration")

        enabled: selectionModel.isEnabled
        opacity: enabled ? 1.0 : ui.theme.itemOpacityDisabled
    }

    Timecode {
        id: durationTimecode

        value: selectionModel.endTime - selectionModel.startTime
        mode: TimecodeModeSelector.Duration

        sampleRate: selectionModel.sampleRate
        tempo: selectionModel.tempo
        upperTimeSignature: selectionModel.upperTimeSignature
        lowerTimeSignature: selectionModel.lowerTimeSignature

        currentFormat: selectionModel.durationFormat

        enabled: selectionModel.isEnabled

        navigation.panel: root.navigationPanel
        navigation.row: startEndTimeCode.navigationRow
        navigation.column: startEndTimeCode.navigationColumnEnd + 1

        accessibleName: durationLabel.text

        onValueChangeRequested: function (newValue) {
            selectionModel.endTime = selectionModel.startTime + newValue
        }

        onCurrentFormatChanged: function () {
            selectionModel.durationFormat = currentFormat
        }
    }
}
