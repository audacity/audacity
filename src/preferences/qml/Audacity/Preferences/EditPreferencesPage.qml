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

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }

        SeparatorLine {}

        DeleteBehaviorSection {
            id: deleteBehaviorSection

            editPreferencesModel: editPreferencesModel
            parentBackgroundColor: root.color

            navigation.section: root.navigationSection
            navigation.order: effectBehaviorSection.navigationOrderEnd + 1

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }

        SeparatorLine {}

        PasteBehaviorSection {
            id: pasteBehaviorSection

            editPreferencesModel: editPreferencesModel
            parentBackgroundColor: root.color

            navigation.section: root.navigationSection
            navigation.order: deleteBehaviorSection.navigationOrderEnd + 1

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }

        SeparatorLine {}

        AsymmetricStereoHeightsSection {
            id: asymmetricStereoHeightsSection

            editPreferencesModel: editPreferencesModel

            navigation.section: root.navigationSection
            navigation.order: pasteBehaviorSection.navigationOrderEnd + 1

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }

        SeparatorLine {}

        MonoStereoConversionSection {
            id: monoStereoConversionSection

            askBeforeConverting: editPreferencesModel.askBeforeConvertingToMonoOrStereo

            navigation.section: root.navigationSection
            navigation.order: asymmetricStereoHeightsSection.navigationOrderEnd + 1

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }

            onAskBeforeConvertingChanged: {
                editPreferencesModel.askBeforeConvertingToMonoOrStereo = askBeforeConverting
            }
        }
    }
}
