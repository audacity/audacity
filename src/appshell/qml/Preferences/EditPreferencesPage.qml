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
            id: effectBehaviorSection

            editPreferencesModel: editPreferencesModel

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart
        }

        SeparatorLine {}

        DeleteBehaviorSection {
            id: deleteBehaviorSection

            editPreferencesModel: editPreferencesModel
            parentBackgroundColor: root.color

            navigation.section: root.navigationSection
            navigation.order: effectBehaviorSection.navigationOrderEnd + 1
        }

        SeparatorLine {}

        PasteBehaviorSection {
            id: pasteBehaviorSection

            editPreferencesModel: editPreferencesModel
            parentBackgroundColor: root.color

            navigation.section: root.navigationSection
            navigation.order: deleteBehaviorSection.navigationOrderEnd + 1
        }

        SeparatorLine {}

        AsymmetricStereoHeightsSection {
            id: asymmetricStereoHeightsSection

            editPreferencesModel: editPreferencesModel

            navigation.section: root.navigationSection
            navigation.order: pasteBehaviorSection.navigationOrderEnd + 1
        }

        SeparatorLine {}

        MonoStereoConversionSection {
            id: monoStereoConversionSection

            askBeforeConverting: editPreferencesModel.askBeforeConvertingToMonoOrStereo

            navigation.section: root.navigationSection
            navigation.order: asymmetricStereoHeightsSection.navigationOrderEnd + 1

            onAskBeforeConvertingChanged: {
                editPreferencesModel.askBeforeConvertingToMonoOrStereo = askBeforeConverting
            }
        }
    }
}
