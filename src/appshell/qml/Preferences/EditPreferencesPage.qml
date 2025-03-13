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

        MonoStereoConversionSection {
            id: monoStereoConversionSection
            askBeforeConverting: editPreferencesModel.askBeforeConvertingToMonoOrStereo
            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart
            onAskBeforeConvertingChanged: {
                editPreferencesModel.askBeforeConvertingToMonoOrStereo = askBeforeConverting
            }
         }

        SeparatorLine { }
    }
}
