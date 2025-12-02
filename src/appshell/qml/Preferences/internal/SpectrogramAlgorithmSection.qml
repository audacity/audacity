/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.UiComponents 1.0

import Audacity.Spectrogram 1.0

SpectrogramBaseSection {
    id: root

    title: qsTrc("appshell/preferences/spectrogram", "Algorithm")

    required property AbstractSpectrogramSettingsModel settingsModel

    StyledListView {
        id: listView

        model: AlgorithmSectionParameterListModel {
            settingsModel: root.settingsModel
            columnWidth: root.prefsColumnWidth
        }

        spacing: root.mediumSpacing
        width: parent.width
        height: contentHeight

        delegate: ComboBoxWithTitle {
            title: controlLabel
            columnWidth: controlWidth
            model: controlPossibleValues
            currentIndex: controlCurrentIndex

            elide: Text.ElideNone
            wrapMode: Text.NoWrap
            spacing: root.narrowSpacing

            onValueEdited: function (index, value) {
                controlCurrentIndex = index
            }
        }
    }
}
