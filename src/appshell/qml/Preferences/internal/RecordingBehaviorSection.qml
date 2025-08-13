/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Recording behavior")

    property var recordingPreferencesModel: null

    navigationOrderEnd: root.navigation.order

    Column {
        width: parent.width
        spacing: 24

        CheckBox {
            id: checkbox

            width: parent.width

            text: qsTrc("appshell/preferences", "Enable audible input monitoring")

            checked: recordingPreferencesModel.audibleInputMonitoring

            navigation.name: "AudibleInputMonitoringBox"
            navigation.panel: root.navigation

            onClicked: {
                recordingPreferencesModel.audibleInputMonitoring = !checked
            }
        }
    }
}
