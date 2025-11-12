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

    Column {
        width: parent.width
        spacing: ui.theme.extra.space_12

        CheckBox {
            id: micMeteringCheckBox

            width: parent.width

            text: qsTrc("appshell/preferences", "Show mic metering")

            checked: recordingPreferencesModel.isMicMeteringOn

            navigation.name: "MicMeteringCheckBox"
            navigation.panel: root.navigation
            navigation.order: 1

            onClicked: {
                recordingPreferencesModel.isMicMeteringOn = !checked
            }
        }

        CheckBox {
            id: inputMonitoringCheckBox

            width: parent.width

            text: qsTrc("appshell/preferences", "Enable input monitoring")

            checked: recordingPreferencesModel.isInputMonitoringOn

            navigation.name: "InputMonitoringCheckBox"
            navigation.panel: root.navigation
            navigation.order: 2

            onClicked: {
                recordingPreferencesModel.isInputMonitoringOn = !checked
            }
        }
    }
}
