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
            navigation.direction: NavigationPanel.Both
            navigationOrderEnd: workspaceSection.navigation.order

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart

            rowSpacing: 16

            StyledTextLabel {
                text: qsTrc("preferences", "Detect tempo in imported files:")
            }

            RadioButtonGroup {
                width: parent.width
                height: workspaceSection.visible ? 80 + workspaceSection.height : 80

                spacing: musicImportsSection.rowSpacing
                orientation: Qt.Vertical

                Column {
                    width: parent.width
                    spacing: musicImportsSection.columnSpacing

                    RoundedRadioButton {
                        checked: musicPreferencesModel.tempoDetectionPref === TempoDetection.ALWAYS
                        text: qsTrc("preferences", "Always")

                        navigation.name: "TempoDetectionAlwaysRadioBtn"
                        navigation.panel: musicImportsSection.navigation
                        navigation.row: 0

                        onToggled: {
                            musicPreferencesModel.setTempoDetectionPref(TempoDetection.ALWAYS)
                        }
                    }

                    RoundedRadioButton {
                        id: workspaceRadioBtn

                        checked: musicPreferencesModel.tempoDetectionPref === TempoDetection.WORKSPACE_DEPENDENT
                        text: qsTrc("preferences", "Depending on workspace")

                        navigation.name: "TempoDetectionWorkspaceDependentRadioBtn"
                        navigation.panel: musicImportsSection.navigation
                        navigation.row: 1

                        onToggled: {
                            musicPreferencesModel.setTempoDetectionPref(TempoDetection.WORKSPACE_DEPENDENT)
                        }
                    }

                    WorkspacesTempoDetectionSection {
                        id: workspaceSection

                        x: 30

                        visible: workspaceRadioBtn.checked

                        musicPreferencesModel: musicPreferencesModel

                        navigation.section: musicImportsSection.navigation.section
                        navigation.order: musicImportsSection.navigation.order + 1
                    }

                    RoundedRadioButton {
                        checked: musicPreferencesModel.tempoDetectionPref === TempoDetection.NEVER
                        text: qsTrc("preferences", "Never")

                        navigation.name: "TempoDetectionNeverRadioBtn"
                        navigation.panel: musicImportsSection.navigation
                        navigation.row: 3

                        onToggled: {
                            musicPreferencesModel.setTempoDetectionPref(TempoDetection.NEVER)
                        }
                    }
                }
            }
        }

        SeparatorLine {}

        BaseSection {
            id: detectionBehaviorSection

            title: qsTrc("preferences", "When Audacity detects music in imported file")

            navigation.section: root.navigationSection
            navigation.order: musicImportsSection.navigationOrderEnd + 1

            CheckBox {
                width: parent.width

                text: qsTrc("preferences", "Ask me each time")
                checked: musicPreferencesModel.askBeforeSubsequentImport

                navigation.name: "AskBeforeSubsequentImportCheckBox"
                navigation.panel: detectionBehaviorSection.navigation
                navigation.row: 0

                onClicked: {
                    musicPreferencesModel.askBeforeSubsequentImport = !checked
                }
            }
        }
    }
}
