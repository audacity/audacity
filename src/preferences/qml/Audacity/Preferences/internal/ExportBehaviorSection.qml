/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("preferences", "Export behavior")

    navigation.name: "ExportBehaviorSection"

    navigationOrderEnd: root.navigation.order

    required property var exportPreferencesModel

    CheckBox {
        width: parent.width

        text: qsTrc("preferences", "Show ‘How would you like to export?’ dialog")

        checked: root.exportPreferencesModel.askExportLocationType

        navigation.name: "AskExportLocationTypeCheckBox"
        navigation.panel: root.navigation
        navigation.row: 0

        onClicked: {
            root.exportPreferencesModel.askExportLocationType = !checked
        }
    }
}
