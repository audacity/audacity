/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.UiComponents 1.0
import Audacity.Preferences

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Playback performance")
    spacing: 16

    property var playbackPreferencesModel: null

    property var playbackQualityLabels: [
        qsTrc("playback", "Low quality"),
        qsTrc("playback", "Medium quality"),
        qsTrc("playback", "High quality"),
        qsTrc("playback", "Best quality")
    ]

    property var ditheringTypeLabels: [
        qsTrc("playback", "None"),
        qsTrc("playback", "Rectangle"),
        qsTrc("playback", "Triangle"),
        qsTrc("playback", "Shaped")
    ]

    ComboBoxWithTitle {
        title: qsTrc("appshell/preferences", "Playback quality")
        columnWidth: root.columnWidth

        currentIndex: playbackPreferencesModel.currentPlaybackQuality
        model: playbackQualityLabels

        navigation.name: "DefaultSampleRateBox"
        navigation.panel: root.navigation
        navigation.row: 0

        onValueEdited: function(newIndex, newValue) {
            playbackPreferencesModel.setPlaybackQuality(newIndex)
        }
    }

    ComboBoxWithTitle {
        title: qsTrc("appshell/preferences", "Dithering")
        columnWidth: root.columnWidth

        currentIndex: playbackPreferencesModel.currentDithering
        model: ditheringTypeLabels

        navigation.name: "DefaultSampleFormatBox"
        navigation.panel: root.navigation
        navigation.row: 1

        onValueEdited: function(newIndex, newValue) {
            playbackPreferencesModel.setDithering(newIndex)
        }
    }
}
