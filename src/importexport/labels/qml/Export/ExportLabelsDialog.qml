/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.UiComponents 1.0
import Audacity.Export

import "internal"

StyledDialogView {
    id: root

    property var trackId: null

    title: qsTrc("export", "Export labels")

    contentWidth: 608
    contentHeight: 506

    margins: 0

    onNavigationActivateRequested: {
        fileNameField.navigation.requestActive()
    }

    ExportLabelsModel {
        id: exportModel
    }

    Component.onCompleted: {
        exportModel.init(root.trackId)
    }

    QtObject {
        id: prv

        property int dropdownWidth: 336
        property int labelColumnWidth: 146

        property int margins: 16
    }

    ColumnLayout {
        id: mainColumn

        anchors.fill: parent

        spacing: 8

        BaseSection {
            id: fileSection

            Layout.fillWidth: true
            Layout.leftMargin: prv.margins
            Layout.rightMargin: prv.margins
            Layout.topMargin: prv.margins
            Layout.preferredHeight: implicitHeight

            title: qsTrc("export", "File")

            navigation.section: root.navigationSection
            navigation.order: 1

            RowLayout {

                Item {
                    width: prv.labelColumnWidth
                    StyledTextLabel {
                        id: fileNameLabel
                        text: qsTrc("export", "File name")
                        anchors.verticalCenter: parent.verticalCenter
                    }
                }

                TextInputField {
                    id: fileNameField

                    Layout.fillWidth: true
                    Layout.minimumWidth: implicitWidth

                    currentText: exportModel.fileName

                    implicitWidth: prv.dropdownWidth

                    navigation.name: "FileNameFieldBox"
                    navigation.panel: fileSection.navigation
                    navigation.order: 1
                    navigation.accessible.name: fileNameLabel.text + ": " + currentText

                    onTextChanged: function(newTextValue) {
                        exportModel.fileName = newTextValue
                    }
                }
            }

            RowLayout {

                Item {
                    width: prv.labelColumnWidth
                    StyledTextLabel {
                        id: fileTypeLabel
                        text: qsTrc("export", "File type")
                        anchors.verticalCenter: parent.verticalCenter
                    }
                }

                StyledDropdown {
                    id: fileTypeDropdown

                    Layout.preferredWidth: prv.dropdownWidth

                    textRole: "title"
                    valueRole: "type"

                    currentIndex: indexOfValue(exportModel.currentFileType)
                    model: exportModel.fileTypes

                    navigation.name: "FileTypeDropdown"
                    navigation.panel: fileSection.navigation
                    navigation.order: fileNameField.navigation.order + 1
                    navigation.accessible.name: fileTypeLabel.text + ": " + currentText

                    indeterminateText: ""

                    onActivated: function(index, value) {
                        exportModel.currentFileType = value
                    }
                }
            }

            RowLayout {

                Item {
                    width: prv.labelColumnWidth
                    StyledTextLabel {
                        id: folderLabel
                        text: qsTrc("export", "Folder")
                        anchors.verticalCenter: parent.verticalCenter
                    }
                }

                FilePicker {
                    id: folderPicker

                    pickerType: FilePicker.PickerType.Directory
                    pathFieldWidth: prv.dropdownWidth
                    buttonWidth: fileSection.width - prv.labelColumnWidth - parent.spacing - pathFieldWidth - spacing
                    spacing: 8

                    buttonType: FlatButton.Horizontal
                    orientation: Qt.Horizontal

                    path: exportModel.directoryPath

                    navigation: fileSection.navigation
                    navigationRowOrderStart: fileTypeDropdown.navigation.order + 1
                    pathFieldTitle: folderLabel.text

                    onPathEdited: function(newPath) {
                        exportModel.directoryPath = newPath
                    }
                }
            }
        }

        SeparatorLine {}

        BaseSection {
            id: labelTracksSection

            Layout.fillWidth: true
            Layout.leftMargin: prv.margins
            Layout.rightMargin: prv.margins
            Layout.preferredHeight: implicitHeight

            title: qsTrc("export", "Label tracks")

            RowLayout {

                Item {
                    Layout.preferredWidth: prv.labelColumnWidth
                    Layout.alignment: Qt.AlignTop
                    Layout.topMargin: 8
                    StyledTextLabel {
                        text: qsTrc("export", "Included label tracks")
                        anchors.verticalCenter: parent.verticalCenter
                    }
                }

                LabelTracksSelectionView {
                    id: labelTracksSectionContent
                    Layout.preferredWidth: labelTracksSection.width - prv.labelColumnWidth - parent.spacing
                    Layout.preferredHeight: 196

                    itemsModel: exportModel.labelTracks
                    selectedItems: exportModel.selectedTracks

                    itemsViewWidth: prv.dropdownWidth

                    navigationSection: root.navigationSection
                    navigationPanelStartOrder: fileSection.navigation.order + 1

                    onSetSelectedRequested: function(trackId, selected) {
                        exportModel.changeSelectionForTrack(trackId, selected)
                    }

                    onSelectAllRequested: function() {
                        exportModel.selectAllTracks()
                    }

                    onDeselectAllRequested: function() {
                        exportModel.deselectAllTracks()
                    }
                }
            }
        }

        SeparatorLine {}

        ButtonBox {
            id: buttonBox

            Layout.fillWidth: true
            Layout.leftMargin: prv.margins
            Layout.rightMargin: prv.margins
            Layout.bottomMargin: prv.margins
            Layout.preferredHeight: implicitHeight

            navigationPanel.section: root.navigationSection
            navigationPanel.order: labelTracksSectionContent.navigationPanelEndOrder + 1

            FlatButton {
                id: cancelBtn
                text: qsTrc("global", "Cancel")
                buttonRole: ButtonBoxModel.RejectRole
                buttonId: ButtonBoxModel.Cancel
                minWidth: 80

                navigation.panel: buttonBox.navigationPanel
                navigation.order: 1

                onClicked: {
                    root.reject()
                }
            }

            FlatButton {
                id: okBtn
                text: qsTrc("global", "Export")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.Apply
                minWidth: 80
                accentButton: true
                enabled: (exportModel.selectedTracks && exportModel.selectedTracks.length > 0)
                         && (exportModel.fileName && exportModel.fileName.trim() !== "")
                         && (exportModel.directoryPath && exportModel.directoryPath.trim() !== "")

                navigation.panel: buttonBox.navigationPanel
                navigation.order: 2

                onClicked: {
                    exportModel.exportData()
                    root.accept()
                }
            }
        }
    }
}
