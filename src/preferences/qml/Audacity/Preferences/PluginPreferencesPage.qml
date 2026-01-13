/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick

import Muse.Ui
import Muse.UiComponents

PreferencesPage {
    id: root

    PluginPreferencesModel {
        id: pluginPreferencesModel
    }

    Component.onCompleted: {
        pluginPreferencesModel.init()
    }

    Column {

        width: parent.width
        spacing: root.sectionsSpacing

        EffectOptionsSection {
            id: effectBehaviorSection

            pluginPreferencesModel: pluginPreferencesModel

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart
        }

        SeparatorLine { }
    }
}
