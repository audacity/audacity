/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick

import Muse.UiComponents

import Audacity.Preferences
import Audacity.UiComponents 1.0

PreferencesPage {
    id: root

    MusicPreferencesModel {
        id: musicPreferencesModel
    }

    Component.onCompleted: {
        musicPreferencesModel.init()
    }

    Column {
        width: parent.width
        spacing: root.sectionsSpacing

        BaseSection {
            id: musicImportsSection

            title: qsTrc("preferences", "Music imports")

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart

            CheckBox {
                width: parent.width

                text: qsTrc("preferences", "Detect tempo in imported files")
                checked: musicPreferencesModel.tempoDetectionEnabled

                navigation.name: "TempoDetectionCheckBox"
                navigation.panel: musicImportsSection.navigation
                navigation.row: 0

                onClicked: {
                    musicPreferencesModel.setTempoDetectionEnabled(!checked)
                }
            }
        }
    }
}
