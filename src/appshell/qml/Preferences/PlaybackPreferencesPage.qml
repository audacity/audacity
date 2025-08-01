/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.UiComponents 1.0
import Audacity.Playback 1.0
import Audacity.Preferences 1.0

import "internal"

PreferencesPage {
    id: root

    property int navigationOrderStart: 0

    PlaybackPreferencesModel {
        id: playbackPreferencesModel
    }

    PlaybackStateModel {
        id: playbackState
    }

    Component.onCompleted: {
        playbackPreferencesModel.init()
    }

    Column {
        width: parent.width
        spacing: root.sectionsSpacing

        PlaybackPerformanceSection {
            playbackPreferencesModel: playbackPreferencesModel

            enabled: !(playbackState.isPaused || playbackState.isPlaying)

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart
        }

        SeparatorLine {}

        SoloButtonSection {
            playbackPreferencesModel: playbackPreferencesModel

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 1
        }

        SeparatorLine {}

        CursorSection {
            playbackPreferencesModel: playbackPreferencesModel

            enabled: !(playbackState.isPaused || playbackState.isPlaying)

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 2
        }
    }
}
