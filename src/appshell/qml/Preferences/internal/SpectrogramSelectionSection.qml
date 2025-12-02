/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.UiComponents 1.0

import Audacity.Spectrogram 1.0

SpectrogramBaseSection {
    id: root

    property bool showTitle: true

    title: root.showTitle ? qsTrc("appshell/preferences", "Selection") : ""

    required property AbstractSpectrogramSettingsModel settingsModel

    CheckBox {
        text: qsTrc("appshell/preferences/spectrogram", "Enable spectral selection")
        checked: settingsModel.spectralSelectionEnabled

        onClicked: {
            settingsModel.spectralSelectionEnabled = !settingsModel.spectralSelectionEnabled
        }
    }
}
