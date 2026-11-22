/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("preferences", "Recording behavior")

    property var recordingPreferencesModel: null

    Column {
        width: parent.width
        spacing: 12

        CheckBox {
            id: overdubCheckBox

            width: parent.width

            text: qsTrc("preferences", "Hear other tracks while recording (overdub)")

            checked: recordingPreferencesModel.isOverdubOn

            navigation.name: "OverdubCheckBox"
            navigation.panel: root.navigation
            navigation.order: 1

            onClicked: {
                recordingPreferencesModel.isOverdubOn = !checked
            }
        }

        CheckBox {
            id: inputMonitoringCheckBox

            width: parent.width

            text: qsTrc("preferences", "Turn on input monitoring (hear yourself while recording)")

            checked: recordingPreferencesModel.isInputMonitoringOn

            navigation.name: "InputMonitoringCheckBox"
            navigation.panel: root.navigation
            navigation.order: 2

            onClicked: {
                recordingPreferencesModel.isInputMonitoringOn = !checked
            }
        }

        CheckBox {
            id: recordOnNewTrackCheckBox

            width: parent.width

            text: qsTrc("preferences", "Record on a new track")

            checked: recordingPreferencesModel.recordOnNewTrack

            navigation.name: "RecordOnNewTrackCheckBox"
            navigation.panel: root.navigation
            navigation.order: 3

            onClicked: {
                recordingPreferencesModel.recordOnNewTrack = !checked
            }
        }

        CheckBox {
            id: detectDropoutsCheckBox

            width: parent.width

            //: A dropout is a loss of a short sequence of audio sample data from the recording
            text: qsTrc("preferences", "Detect dropouts")

            checked: recordingPreferencesModel.detectDropouts

            navigation.name: "DetectDropoutsCheckBox"
            navigation.panel: root.navigation
            navigation.order: 4

            onClicked: {
                recordingPreferencesModel.detectDropouts = !checked
            }
        }

        CheckBox {
            id: micMeteringCheckBox

            width: parent.width

            text: qsTrc("preferences", "Show mic metering when not recording")

            checked: recordingPreferencesModel.isMicMeteringOn

            navigation.name: "MicMeteringCheckBox"
            navigation.panel: root.navigation
            navigation.order: 5

            onClicked: {
                recordingPreferencesModel.isMicMeteringOn = !checked
            }
        }
    }
}
