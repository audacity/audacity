/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Audacity.Preferences 1.0

import "internal"

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
