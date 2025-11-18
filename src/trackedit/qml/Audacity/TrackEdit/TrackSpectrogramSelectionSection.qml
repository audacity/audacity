/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.UiComponents 1.0

import Audacity.Spectrogram 1.0
import Preferences // SpectrogramBaseSection

SpectrogramBaseSection {
    id: root

    required property TrackSpectrogramSettingsModel settingsModel

    Column {

        spacing: root.mediumSpacing

        CheckBox {
            text: qsTrc("appshell/preferences/spectrogram", "Use global settings")
            checked: settingsModel.useGlobalSettings

            onClicked: {
                settingsModel.useGlobalSettings = !settingsModel.useGlobalSettings
            }
        }

        CheckBox {
            text: qsTrc("appshell/preferences/spectrogram", "Enable spectral selection")
            checked: settingsModel.spectralSelectionEnabled

            onClicked: {
                settingsModel.spectralSelectionEnabled = !settingsModel.spectralSelectionEnabled
            }
        }
    }
}
