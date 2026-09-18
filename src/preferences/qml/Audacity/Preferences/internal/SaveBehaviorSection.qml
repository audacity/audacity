/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.Preferences
import Audacity.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("preferences", "Save behavior")

    navigation.name: "SaveBehaviorSection"

    navigationOrderEnd: root.navigation.order

    required property var exportPreferencesModel

    Column {
        width: parent.width
        spacing: root.rowSpacing

        RoundedRadioButton {
            checked: root.exportPreferencesModel.saveBehavior === SaveBehavior.AlwaysAsk
            text: qsTrc("preferences", "Always ask")

            navigation.name: "AlwaysAskRadioBtn"
            navigation.panel: root.navigation
            navigation.row: 0

            onToggled: {
                root.exportPreferencesModel.setSaveBehavior(SaveBehavior.AlwaysAsk)
            }
        }

        RoundedRadioButton {
            checked: root.exportPreferencesModel.saveBehavior === SaveBehavior.AlwaysSaveToCloud
            text: qsTrc("preferences", "Always save to cloud")

            navigation.name: "AlwaysSaveToCloudRadioBtn"
            navigation.panel: root.navigation
            navigation.row: 1

            onToggled: {
                root.exportPreferencesModel.setSaveBehavior(SaveBehavior.AlwaysSaveToCloud)
            }
        }

        RoundedRadioButton {
            checked: root.exportPreferencesModel.saveBehavior === SaveBehavior.AlwaysSaveToComputer
            text: qsTrc("preferences", "Always save to computer")

            navigation.name: "AlwaysSaveToComputerRadioBtn"
            navigation.panel: root.navigation
            navigation.row: 2

            onToggled: {
                root.exportPreferencesModel.setSaveBehavior(SaveBehavior.AlwaysSaveToComputer)
            }
        }
    }
}
