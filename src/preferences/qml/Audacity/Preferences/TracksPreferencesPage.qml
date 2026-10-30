/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.AppShell
import Audacity.Preferences

PreferencesPage {
    id: root

    TracksPreferencesModel {
        id: tracksPreferencesModel
    }

    Component.onCompleted: {
        tracksPreferencesModel.init()
    }

    Column {
        width: parent.width
        spacing: root.sectionsSpacing

        TrackDisplaySection {
            id: trackDisplaySection

            tracksPreferencesModel: tracksPreferencesModel

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }
    }
}
