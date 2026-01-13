/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.UiComponents

import Audacity.UiComponents 1.0
import Audacity.Spectrogram 1.0

SpectrogramBaseSection {
    id: root

    title: qsTrc("preferences/spectrogram", "Algorithm")

    required property AbstractSpectrogramSettingsModel settingsModel

    StyledListView {
        id: listView

        model: AlgorithmSectionParameterListModel {
            settingsModel: root.settingsModel
            columnWidth: root.prefsColumnWidth
        }

        clip: false // or the highlight rectangle will be clipped
        spacing: root.mediumSpacing
        width: parent.width
        height: contentHeight

        delegate: ComboBoxWithTitle {
            navigation.panel: root.navigation
            navigation.order: index
            navigation.name: "AlgorithmComboBox_" + index

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
