/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.UiComponents 1.0

import Audacity.Spectrogram 1.0

SpectrogramBaseSection {
    id: root

    title: qsTrc("appshell/preferences/spectrogram", "Scale")

    required property AbstractSpectrogramSettingsModel settingsModel

    ComboBoxWithTitle {
        width: parent.width

        title: qsTrc("spectrogram/preferences/settings", "Scale")
        spacing: root.narrowSpacing
        columnWidth: root.mediumControlWidth
        model: settingsModel.scaleNames
        currentIndex: settingsModel.scale

        onValueEdited: function (index) {
            settingsModel.scale = index
        }
    }
}
