/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.UiComponents 1.0
import Audacity.Preferences

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Effect menu organization")
    spacing: 16

    property var pluginPreferencesModel: null

    RadioButtonGroup {

        width: parent.width
        height: 50

        spacing: root.rowSpacing
        orientation: Qt.Vertical

        Column {
            width: parent.width
            spacing: root.columnSpacing

            RoundedRadioButton {

                checked: pluginPreferencesModel.effectMenuOrganization == 0
                text: qsTrc("appshell/preferences", "Group effects")

                navigation.name: "GroupEffects"
                navigation.panel: root.navigation
                navigation.row: 0

                onToggled: {
                    pluginPreferencesModel.setEffectMenuOrganization(0)
                }
            }

            RoundedRadioButton {
                checked: pluginPreferencesModel.effectMenuOrganization == 1
                text: qsTrc("appshell/preferences", "Display effects in one flat list")

                navigation.name: "DoNotGroupEffects"
                navigation.panel: root.navigation
                navigation.row: 1

                onToggled: {
                    pluginPreferencesModel.setEffectMenuOrganization(1)
                }
            }
        }
    }
}
