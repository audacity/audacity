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

    EditPreferencesModel {
        id: editPreferencesModel
    }

    Component.onCompleted: {
        editPreferencesModel.init()
    }

    Column {

        width: parent.width
        spacing: root.sectionsSpacing

        EffectBehaviorSection {
            editPreferencesModel: editPreferencesModel

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart
        }

        SeparatorLine { }

        AsymmetricStereoHeightsSection {
            editPreferencesModel: editPreferencesModel

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 1
        }

        SeparatorLine { }

        PastingBehaviorSection {
            editPreferencesModel: editPreferencesModel

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 3 // skip one as there's navi for workspaces

        }

        SeparatorLine { }

        MonoStereoConversionSection {
            id: monoStereoConversionSection

            askBeforeConverting: editPreferencesModel.askBeforeConvertingToMonoOrStereo

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 4

            onAskBeforeConvertingChanged: {
                editPreferencesModel.askBeforeConvertingToMonoOrStereo = askBeforeConverting
            }
         }
    }
}
