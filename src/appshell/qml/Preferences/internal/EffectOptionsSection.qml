/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.UiComponents 1.0
import Audacity.Preferences

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Effect options")
    spacing: 16

    property var pluginPreferencesModel: null

    property var effectOrganizationLabels: [
        qsTrc("playback", "Group by category"),
        qsTrc("playback", "Group by type"),
    ]

    ComboBoxWithTitle {
        title: qsTrc("appshell/preferences", "Effect menu organization")
        columnWidth: root.columnWidth

        currentIndex: pluginPreferencesModel.effectMenuOrganization
        model: effectOrganizationLabels

        navigation.name: "EffectMenuOrganizationBox"
        navigation.panel: root.navigation
        navigation.row: 0

        onValueEdited: function(newIndex, newValue) {
            pluginPreferencesModel.setEffectMenuOrganization(newIndex)
        }
    }

    ComboBoxWithTitle {
        title: qsTrc("appshell/preferences", "Realtime effect organization")
        columnWidth: root.columnWidth

        currentIndex: pluginPreferencesModel.realtimeEffectOrganization
        model: effectOrganizationLabels

        navigation.name: "RealtimeEffectOrganizationBox"
        navigation.panel: root.navigation
        navigation.row: 1

        onValueEdited: function(newIndex, newValue) {
            pluginPreferencesModel.setRealtimeEffectOrganization(newIndex)
        }
    }
}
