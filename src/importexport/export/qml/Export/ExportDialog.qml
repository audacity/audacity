import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Muse.Ui
import Muse.UiComponents

import Audacity.Export 1.0
import "internal"

StyledDialogView {
    id: root

    title: qsTrc("export", "Export audio")

    contentWidth: 480
    // TODO: switch to mainColumn.implicitHeight when
    // dynamic content is implemented
    // contentHeight: mainColumn.implicitHeight
    contentHeight: 495

    margins: 20

    property int dropdownWidth: 354
    property int labelColumnWidth: 80

    // property NavigationPanel navigation: NavigationPanel {
    //     name: root.title
    //     enabled: root.enabled && root.visible
    //     direction: NavigationPanel.Horizontal
    //     section: root.navigationSection
    // }

    ExportPreferencesModel {
        id: exportPreferencesModel
    }

    ColumnLayout {
        id: mainColumn

        spacing: 16

        BaseSection {
            title: qsTrc("export", "Export")

            spacing: 10

            RowLayout {

                Item {
                    width: root.labelColumnWidth

                    StyledTextLabel {
                        text: qsTrc("export", "Process")

                        anchors.verticalCenter: parent.verticalCenter
                    }
                }

                StyledDropdown {
                    id: processDropdown

                    Layout.preferredWidth: root.dropdownWidth

                    textRole: "name"
                    valueRole: "code"

                    popupItemsCount: 11
                    currentIndex: indexOfValue(exportPreferencesModel.currentProcess)
                    model: exportPreferencesModel.processList

                    // navigation.name: "ProcessBox"
                    // navigation.panel: root.navigation
                    // navigation.order: 0
                    // navigation.row: 0

                    indeterminateText: ""

                    onActivated: function(index, value) {
                        exportPreferencesModel.setCurrentProcess(value)
                    }
                }
            }
        }

        BaseSection {
            title: qsTrc("export", "File")

            RowLayout {

                Item {
                    width: root.labelColumnWidth
                    StyledTextLabel {
                        text: qsTrc("export", "File name")
                        anchors.verticalCenter: parent.verticalCenter
                    }
                }

                TextInputField {
                    id: filenameField

                    Layout.fillWidth: true
                    Layout.minimumWidth: implicitWidth

                    currentText: exportPreferencesModel.filename
                    measureUnitsSymbol: "." + exportPreferencesModel.fileExtension

                    implicitWidth: root.dropdownWidth

                    // navigation.name: "FilenameFieldBox"
                    // navigation.panel: root.navigation
                    // navigation.order: 1
                    // navigation.row: 1

                    onTextChanged: function(newTextValue) {
                        exportPreferencesModel.setFilename(newTextValue)
                    }
                }
            }

            RowLayout {

                Item {
                    width: root.labelColumnWidth
                    StyledTextLabel {
                        text: qsTrc("export", "Folder")
                        anchors.verticalCenter: parent.verticalCenter
                    }
                }

                FilePicker {
                    id: dirPicker

                    pickerType: FilePicker.PickerType.Directory
                    pathFieldWidth: root.dropdownWidth
                    spacing: 10

                    path: exportPreferencesModel.directoryPath
                    dir: exportPreferencesModel.directoryPath

                    // navigation: root.navigation
                    // navigationRowOrderStart: 2
                    // navigation.order: 2
                    // navigation.order: filenameField.navigation.order + 1

                    onPathEdited: function(newPath) {
                        path = newPath
                        exportPreferencesModel.setDirectoryPath(newPath)
                    }
                }
            }

            RowLayout {

                Item {
                    width: root.labelColumnWidth
                    StyledTextLabel {
                        text: qsTrc("export", "Format")
                        anchors.verticalCenter: parent.verticalCenter
                    }
                }

                StyledDropdown {
                    id: formatDropdown

                    Layout.preferredWidth: root.dropdownWidth * 0.6

                    textRole: "name"
                    valueRole: "code"

                    popupItemsCount: 11
                    currentIndex: indexOfValue(exportPreferencesModel.currentFormat)
                    model: exportPreferencesModel.formatsList

                    // navigation.name: "FormatBox"
                    // navigation.panel: root.navigation
                    // navigation.order: dirPicker.navigation.order + 1

                    indeterminateText: ""

                    onActivated: function(index, value) {
                        exportPreferencesModel.setCurrentFormat(value)
                    }
                }
            }
        }

        BaseSection {
            title: qsTrc("export", "Audio options")

            RowLayout {

                Item {
                    id: spacer

                    width: root.labelColumnWidth
                }

                RadioButtonGroup {
                    id: channelsGroup

                    spacing: root.rowSpacing
                    orientation: Qt.Horizontal

                    Layout.preferredWidth: root.dropdownWidth
                    Layout.preferredHeight: 20

                    Row {
                        width: parent.width
                        spacing: 10

                        RoundedRadioButton {
                            id: monoBtn

                            checked: exportPreferencesModel.exportChannels == ExportChannels.MONO
                            enabled: exportPreferencesModel.maxExportChannels > 0
                            text: qsTrc("export", "Mono")

                            // navigation.name: "MonoBox"
                            // navigation.panel: root.navigation
                            // navigation.order: formatDropdown.navigation.order + 1
                            // navigation.row: 0

                            onToggled: {
                                exportPreferencesModel.setExportChannels(ExportChannels.MONO)
                            }
                        }

                        RoundedRadioButton {

                            checked: exportPreferencesModel.exportChannels == ExportChannels.STEREO
                            enabled: exportPreferencesModel.maxExportChannels > 1
                            text: qsTrc("export", "Stereo")

                            // navigation.name: "StereoBox"
                            // navigation.panel: root.navigation
                            // navigation.order: formatDropdown.navigation.order + 1
                            // navigation.row: 1

                            onToggled: {
                                exportPreferencesModel.setExportChannels(ExportChannels.STEREO)
                            }
                        }

                        RoundedRadioButton {

                            checked: exportPreferencesModel.exportChannels == ExportChannels.CUSTOM
                            text: qsTrc("export", "Custom mapping")
                            enabled: false // until custom mapping grid is implemented

                            // navigation.name: "CustomBox"
                            // navigation.panel: root.navigation
                            // navigation.order: formatDropdown.navigation.order + 1
                            // navigation.row: 2

                            onToggled: {
                                exportPreferencesModel.setExportChannels(ExportChannels.CUSTOM)
                            }
                        }
                    }
                }
            }

            RowLayout {

                Item {
                    width: root.labelColumnWidth
                    StyledTextLabel {
                        text: qsTrc("export", "Sample rate")
                        anchors.verticalCenter: parent.verticalCenter
                    }
                }

                StyledDropdown {
                    id: sampleRateDropdown

                    Layout.preferredWidth: root.dropdownWidth * 0.6

                    textRole: "name"
                    valueRole: "code"

                    popupItemsCount: 11
                    currentIndex: indexOfValue(exportPreferencesModel.exportSampleRate)
                    model: exportPreferencesModel.exportSampleRateList

                    // navigation.name: "ProcessBox"
                    // navigation.panel: root.navigation
                    // navigation.column: 1

                    indeterminateText: ""

                    onActivated: function(index, value) {
                        exportPreferencesModel.setExportSampleRate(value)
                    }


                }
            }

            RowLayout {

                Item {
                    width: root.labelColumnWidth
                    StyledTextLabel {
                        text: qsTrc("export", "Encoding")
                        anchors.verticalCenter: parent.verticalCenter
                    }
                }

                StyledDropdown {
                    id: encodingDropdown

                    Layout.preferredWidth: root.dropdownWidth * 0.6

                    textRole: "name"
                    valueRole: "code"

                    popupItemsCount: 11
                    currentIndex: 2
                    model: [ "16-bit", "24-bit", "32-bit float"]
                    enabled: false

                    // navigation.name: "ProcessBox"
                    // navigation.panel: root.navigation
                    // navigation.column: 1

                    indeterminateText: ""

                    onActivated: function(index, value) {
                    }
                }
            }
        }

        BaseSection {
            title: qsTrc("export", "Rendering")

            ColumnLayout {
                CheckBox {
                    id: trimBlankSpaceCheckBox
                    width: parent.width

                    text: qsTrc("export", "Trim blank space before first clip")
                    enabled: false

                    // navigation.name: "TrimBlankSpaceBox"
                    // navigation.panel: root.navigation
                    // navigation.row: 1
                    // navigation.column: 0

                    onClicked: {}
                }
            }
        }

        SeparatorLine {
            width: root.contentWidth
        }

        ButtonBox {
            id: buttonBox

            Layout.fillWidth: true

            FlatButton {
                text: qsTrc("appshell/preferences", "Edit metadata")
                buttonRole: ButtonBoxModel.CustomRole
                buttonId: ButtonBoxModel.CustomButton + 1
                isLeftSide: true
                enabled: false

                onClicked: {
                }
            }

            FlatButton {
                id: cancelBtn
                text: qsTrc("global", "Cancel")
                buttonRole: ButtonBoxModel.RejectRole
                buttonId: ButtonBoxModel.Cancel
                minWidth: 80
                onClicked: root.reject()
            }

            FlatButton {
                id: okBtn
                text: qsTrc("global", "Export")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.Apply
                minWidth: 80
                accentButton: true
                onClicked: {
                    if (exportPreferencesModel.verifyExportPossible()) {
                        exportPreferencesModel.exportData()
                        root.accept()
                    }
                }
            }
        }

    }

}
