/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.UiComponents 1.0

BaseSection {
    id: root

    required property var preferenceGroup
    required property var pluginPreferencesModel

    title: preferenceGroup.title

    navigation.direction: NavigationPanel.Both

    Repeater {
        model: root.preferenceGroup.items

        delegate: Column {
            id: preferenceRow

            required property var modelData
            required property int index

            width: parent.width
            spacing: 6

            StyledTextLabel {
                width: parent.width
                text: preferenceRow.modelData.title
                horizontalAlignment: Text.AlignLeading
                wrapMode: Text.WordWrap
            }

            Loader {
                id: editor

                width: 360
                sourceComponent: preferenceRow.modelData.type === "bool" ? booleanEditor : preferenceRow.modelData.type === "enum" ? enumEditor : preferenceRow.modelData.type === "directory" ? directoryEditor : textEditor

                property var preference: preferenceRow.modelData
                property int row: preferenceRow.index
                property var section: root
            }

            StyledTextLabel {
                width: parent.width
                visible: text.length > 0
                text: preferenceRow.modelData.description
                horizontalAlignment: Text.AlignLeading
                wrapMode: Text.WordWrap
                color: ui.theme.fontSecondaryColor
            }
        }
    }

    Component {
        id: booleanEditor

        CheckBox {
            id: checkBox

            readonly property var preference: parent.preference
            readonly property var section: parent.section

            checked: preference.value
            navigation.name: section.preferenceGroup.extensionId + "." + preference.id
            navigation.panel: section.navigation
            navigation.row: parent.row
            navigation.column: 0
            onClicked: {
                checked = !checked
                section.pluginPreferencesModel.setExtensionPreference(section.preferenceGroup.extensionId, preference.id, checked)
            }
        }
    }

    Component {
        id: enumEditor

        StyledDropdown {
            id: dropdown

            readonly property var preference: parent.preference
            readonly property var section: parent.section

            width: 280
            model: preference.choices
            textRole: "text"
            valueRole: "value"
            currentIndex: indexOfValue(preference.value)
            navigation.name: section.preferenceGroup.extensionId + "." + preference.id
            navigation.panel: section.navigation
            navigation.row: parent.row
            navigation.column: 0
            onActivated: function (index, value) {
                currentIndex = index
                section.pluginPreferencesModel.setExtensionPreference(section.preferenceGroup.extensionId, preference.id, value)
            }
        }
    }

    Component {
        id: textEditor

        TextInputField {
            id: textInput

            readonly property var preference: parent.preference
            readonly property var section: parent.section

            width: 360
            currentText: preference.value ?? ""
            navigation.name: section.preferenceGroup.extensionId + "." + preference.id
            navigation.panel: section.navigation
            navigation.row: parent.row
            navigation.column: 0
            onTextEditingFinished: function (value) {
                section.pluginPreferencesModel.setExtensionPreference(section.preferenceGroup.extensionId, preference.id, value)
            }
        }
    }

    Component {
        id: directoryEditor

        FilePicker {
            readonly property var preference: parent.preference
            readonly property var section: parent.section

            width: 360
            pickerType: FilePicker.PickerType.Directory
            path: preference.value ?? ""
            pathFieldTitle: preference.title
            dialogTitle: preference.title
            navigation: section.navigation
            navigationRowOrderStart: parent.row

            onPathEdited: function (value) {
                path = value
                section.pluginPreferencesModel.setExtensionPreference(section.preferenceGroup.extensionId, preference.id, value)
            }
        }
    }
}
