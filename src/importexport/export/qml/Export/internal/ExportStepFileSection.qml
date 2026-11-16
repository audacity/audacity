/*
* Audacity: A Digital Audio Editor
*/
pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.UiComponents 1.0

BaseSection {
    id: root

    required property var stepModel
    property int labelColumnWidth: 140
    property int itemSpacing: 8

    function requestFileNameActive() {
        fileNameField.navigation.requestActive()
    }

    //: Section heading in the dialog of the export macro step
    title: qsTrc("export", "File")

    FilePickerModel {
        id: filePickerModel

        title: qsTrc("export", "Choose the output file")
        filter: root.stepModel.fileFilter
        dir: root.stepModel.folder !== "" ? root.stepModel.folder : root.stepModel.defaultFolder
    }

    component RowLabel: Item {
        id: rowLabel

        property alias text: label.text

        Layout.preferredWidth: root.labelColumnWidth
        Layout.fillHeight: true

        StyledTextLabel {
            id: label

            anchors.verticalCenter: parent.verticalCenter
            horizontalAlignment: Text.AlignLeft
        }
    }

    RowLayout {
        width: parent.width
        spacing: root.itemSpacing

        RowLabel {
            id: fileNameLabel
            text: qsTrc("export", "File name")
        }

        TextInputField {
            id: fileNameField

            Layout.fillWidth: true

            currentText: root.stepModel.fileName
            hint: "{ProjectName}"

            navigation.name: "FileNameField"
            navigation.panel: root.navigation
            navigation.order: 1
            navigation.accessible.name: fileNameLabel.text + ": " + currentText

            onTextChanged: function (newTextValue) {
                root.stepModel.fileName = newTextValue
            }
        }
    }

    RowLayout {
        width: parent.width
        spacing: root.itemSpacing

        RowLabel {
            id: folderLabel
            text: qsTrc("export", "Folder")
        }

        TextInputField {
            id: folderField

            Layout.fillWidth: true

            currentText: root.stepModel.folder
            hint: root.stepModel.defaultFolder

            navigation.name: "FolderField"
            navigation.panel: root.navigation
            navigation.order: 2
            navigation.accessible.name: folderLabel.text + ": " + currentText

            onTextChanged: function (newTextValue) {
                root.stepModel.folder = newTextValue
            }
        }

        FlatButton {
            icon: IconCode.OPEN_FILE
            text: qsTrc("ui", "Browse")

            buttonType: FlatButton.Horizontal
            orientation: Qt.Horizontal

            navigation.name: "BrowseButton"
            navigation.panel: root.navigation
            navigation.order: 3

            onClicked: {
                const selectedFile = filePickerModel.selectAny()
                if (Boolean(selectedFile)) {
                    root.stepModel.setPickedFile(selectedFile)
                }
            }
        }
    }

    RowLayout {
        width: parent.width
        spacing: root.itemSpacing

        RowLabel {
            text: qsTrc("export", "Preview")
        }

        StyledTextLabel {
            Layout.fillWidth: true

            text: root.stepModel.preview
            horizontalAlignment: Text.AlignLeft
            elide: Text.ElideMiddle
        }
    }

    RowLayout {
        width: parent.width
        spacing: root.itemSpacing
        visible: root.stepModel.fileNameWarning !== ""

        RowLabel {}

        StyledTextLabel {
            Layout.fillWidth: true

            text: root.stepModel.fileNameWarning
            color: ui.theme.extra["error_text_color"]
            horizontalAlignment: Text.AlignLeft
            wrapMode: Text.WordWrap
        }
    }
}
