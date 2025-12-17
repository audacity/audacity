/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.UiComponents 1.0

import Audacity.Spectrogram 1.0

TrackSpectrogramBaseSection {
    id: root

    required property TrackSpectrogramSettingsModel settingsModel

    Column {

        spacing: root.mediumSpacing

        CheckBox {
            text: qsTrc("appshell/preferences/spectrogram", "Use global settings")
            checked: settingsModel.useGlobalSettings

            navigation.panel: root.navigation
            navigation.name: "UseGlobalSettingsCheckBox"
            navigation.order: 0

            onClicked: {
                settingsModel.useGlobalSettings = !settingsModel.useGlobalSettings
            }
        }
    }
}
