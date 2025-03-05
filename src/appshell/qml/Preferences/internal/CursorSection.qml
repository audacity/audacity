/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Audacity.Preferences

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Move cursor along the timeline during playback")
    spacing: 16

    property var playbackPreferencesModel: null

    navigation.direction: NavigationPanel.Both

    IncrementalPropertyControlWithTitle {
        title: qsTrc("appshell/preferences", "Short skip")

        currentValue: playbackPreferencesModel.shortSkip

        columnWidth: root.columnWidth
        controlWidth: 120
        spacing: root.columnSpacing

        measureUnitsSymbol: qsTrc("global", " seconds")

        navigation.name: "ShortSkipControl"
        navigation.panel: root.navigation
        navigation.row: 0

        onValueEdited: function(newValue) {
            playbackPreferencesModel.setShortSkip(newValue)
        }
    }

    IncrementalPropertyControlWithTitle {
        title: qsTrc("appshell/preferences", "Long skip")

        currentValue: playbackPreferencesModel.longSkip

        columnWidth: root.columnWidth
        controlWidth: 120
        spacing: root.columnSpacing

        measureUnitsSymbol: qsTrc("global", " seconds")

        navigation.name: "LongSkipControl"
        navigation.panel: root.navigation
        navigation.row: 1

        onValueEdited: function(newValue) {
            playbackPreferencesModel.setLongSkip(newValue)
        }
    }
}
