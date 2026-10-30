/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("preferences", "Display")

    required property var tracksPreferencesModel

    navigation.direction: NavigationPanel.Both

    function deselectTrackName() {
        // Changing a dropdown can hand focus to the text field, which selects all of its text
        Qt.callLater(function () {
            trackNameField.inputField.deselect()
        })
    }

    Column {
        spacing: 24

        ComboBoxWithTitle {
            id: trackVisualizationBox

            title: qsTrc("preferences", "Default track visualization")
            columnWidth: root.columnWidth

            model: root.tracksPreferencesModel.trackVisualizations
            control.textRole: "title"
            control.valueRole: "value"

            currentIndex: control.indexOfValue(root.tracksPreferencesModel.defaultTrackVisualization)

            navigation.name: "DefaultTrackVisualizationBox"
            navigation.panel: root.navigation
            navigation.row: 0
            navigation.column: 0

            onValueEdited: function (newIndex, newValue) {
                root.tracksPreferencesModel.setDefaultTrackVisualization(newValue)
                root.deselectTrackName()
            }
        }

        ComboBoxWithTitle {
            id: rulerFormatBox

            title: qsTrc("preferences", "Default ruler format")
            columnWidth: root.columnWidth

            model: root.tracksPreferencesModel.rulerFormats
            control.textRole: "title"
            control.valueRole: "value"

            currentIndex: control.indexOfValue(root.tracksPreferencesModel.defaultRulerFormat)

            navigation.name: "DefaultRulerFormatBox"
            navigation.panel: root.navigation
            navigation.row: 1
            navigation.column: 0

            onValueEdited: function (newIndex, newValue) {
                root.tracksPreferencesModel.setDefaultRulerFormat(newValue)
                root.deselectTrackName()
            }
        }

        Column {
            width: root.columnWidth
            spacing: 6

            StyledTextLabel {
                id: trackNameLabel

                width: parent.width
                horizontalAlignment: Qt.AlignLeft
                text: qsTrc("preferences", "Default audio track name")
            }

            TextInputField {
                id: trackNameField

                width: parent.width

                currentText: root.tracksPreferencesModel.defaultTrackName

                navigation.name: "DefaultTrackNameField"
                navigation.panel: root.navigation
                navigation.row: 2
                navigation.column: 0
                navigation.accessible.name: trackNameLabel.text + " " + currentText

                onTextEditingFinished: function (newTextValue) {
                    root.tracksPreferencesModel.setDefaultTrackName(newTextValue)
                    // The model falls back to the default name when the field is emptied
                    inputField.text = root.tracksPreferencesModel.defaultTrackName
                }
            }
        }
    }
}
