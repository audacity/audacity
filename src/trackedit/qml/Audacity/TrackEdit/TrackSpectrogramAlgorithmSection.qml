/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.UiComponents 1.0

import Audacity.Spectrogram 1.0

TrackSpectrogramBaseSection {
    id: root

    title: qsTrc("appshell/preferences/spectrogram", "Algorithm")

    required property TrackSpectrogramSettingsModel settingsModel

    StyledListView {
        id: listView

        model: AlgorithmSectionParameterListModel {
            settingsModel: root.settingsModel
            columnWidth: root.prefsColumnWidth
        }

        spacing: root.narrowSpacing
        width: parent.width
        height: contentHeight
        clip: false // or the highlight rectangle will be clipped

        delegate: Row {
            spacing: 0

            StyledTextLabel {
                width: root.mediumControlWidth
                anchors.verticalCenter: parent.verticalCenter

                text: controlLabel
                elide: Text.ElideNone
                wrapMode: Text.NoWrap
                horizontalAlignment: Text.AlignLeft
            }

            StyledDropdown {
                id: comboBox
                width: controlWidth

                navigation.panel: root.navigation
                navigation.order: index
                navigation.name: "AlgorithmComboBox_" + index

                model: controlPossibleValues
                currentIndex: controlCurrentIndex
                onActivated: function (index, value) {
                    controlCurrentIndex = index
                }
            }
        }
    }
}
