/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("preferences", "Punch and roll recording")
    spacing: 16

    property var recordingPreferencesModel: null

    navigation.direction: NavigationPanel.Both

    IncrementalPropertyControlWithTitle {
        title: qsTrc("preferences", "Pre-roll duration")

        currentValue: recordingPreferencesModel.leadInTimeDuration

        columnWidth: root.columnWidth
        controlWidth: 120
        spacing: root.columnSpacing

        minValue: 0
        maxValue: 30
        step: 0.5
        decimals: 1

        measureUnitsSymbol: " " + qsTrc("global", "seconds")

        navigation.name: "LeadInTimeDurationControl"
        navigation.panel: root.navigation
        navigation.row: 0

        onValueEdited: function (newValue) {
            recordingPreferencesModel.setLeadInTimeDuration(newValue)
        }
    }

    IncrementalPropertyControlWithTitle {
        title: qsTrc("preferences", "Crossfade duration")

        currentValue: recordingPreferencesModel.crossfadeDuration

        columnWidth: root.columnWidth
        controlWidth: 120
        spacing: root.columnSpacing

        minValue: 0
        maxValue: 100
        step: 1
        decimals: 1

        measureUnitsSymbol: " " + qsTrc("global", "ms")

        navigation.name: "CrossfadeDurationControl"
        navigation.panel: root.navigation
        navigation.row: 1

        onValueEdited: function (newValue) {
            recordingPreferencesModel.setCrossfadeDuration(newValue)
        }
    }
}
