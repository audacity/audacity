/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.UiComponents 1.0
import Audacity.Record 1.0

BaseSection {
    id: root

    title: qsTrc("preferences", "Sound activated recording")

    property var recordingPreferencesModel: null

    CheckBox {
        width: parent.width

        text: qsTrc("preferences", "Turn on sound activated recording")

        checked: recordingPreferencesModel.isSoundActivatedRecordingOn

        navigation.name: "SoundActivatedRecordingCheckBox"
        navigation.panel: root.navigation
        navigation.order: 1

        onClicked: {
            recordingPreferencesModel.isSoundActivatedRecordingOn = !checked
        }
    }

    SoundActivationLevelControl {
        width: parent.width

        enabled: recordingPreferencesModel.isSoundActivatedRecordingOn
        applyImmediately: true
        listening: enabled && visible

        navigationPanel: root.navigation
        navigationOrderStart: 2
    }
}
