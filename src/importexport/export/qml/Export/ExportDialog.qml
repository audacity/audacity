/*
* Audacity: A Digital Audio Editor
*/
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

    contentWidth: 490
    contentHeight: mainColumn.implicitHeight

    margins: 10

    property int dropdownWidth: 364
    property int smallDropdownWidth: 364 * 0.65
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

    CustomFFmpegPreferencesModel {
        id: ffmpegPrefModel
    }

    DynamicExportOptionsModel {
        id: dynamicOptionsModel
    }

    Component.onCompleted: {
        // NOTE: position dialog so entire content is visible
        y = -250

        exportPreferencesModel.init()
        ffmpegPrefModel.init()
        dynamicOptionsModel.init()
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

                    Layout.preferredWidth: root.smallDropdownWidth

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

                    Layout.preferredWidth: root.smallDropdownWidth

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

            ColumnLayout {

                visible: exportPreferencesModel.customFFmpegOptionsVisible

                RowLayout {

                    Item {
                        width: root.labelColumnWidth
                    }

                    FlatButton {

                        Layout.preferredWidth: root.smallDropdownWidth

                        text: qsTrc("export", "Open custom FFmpeg format options")

                        onClicked: {
                            exportPreferencesModel.openCustomFFmpegDialog()
                        }
                    }
                }

                RowLayout {

                    Item {

                        width: root.labelColumnWidth

                        StyledTextLabel {
                            anchors.verticalCenter: parent.verticalCenter

                            text: qsTrc("export", "Format: ")
                        }
                    }

                    TextInputField {
                        id: formatField

                        implicitWidth: root.smallDropdownWidth

                        currentText: ffmpegPrefModel.ffmpegFormat

                        readOnly: true
                    }
                }

                RowLayout {

                    Item {

                        width: root.labelColumnWidth

                        StyledTextLabel {
                            anchors.verticalCenter: parent.verticalCenter

                            text: qsTrc("export", "Codec:")
                        }
                    }

                    TextInputField {
                        id: codecField

                        implicitWidth: root.smallDropdownWidth

                        currentText: ffmpegPrefModel.ffmpegCodec

                        readOnly: true
                    }
                }
            }

            ColumnLayout {
                id: dynamicSection

                spacing: 10

                visible: !exportPreferencesModel.customFFmpegOptionsVisible

                Repeater {

                    model: dynamicOptionsModel

                    delegate: RowLayout {
                        Layout.fillWidth: true
                        Layout.preferredHeight: 30

                        visible: !model.hidden
                        enabled: !model.readOnly

                        property var option: ({
                            index: model.index,
                            type: model.type,
                            value: model.value,
                            values: model.values,
                            names: model.names,
                            min: model.min,
                            max: model.max
                        })

                        Item {
                            width: root.labelColumnWidth

                            StyledTextLabel {
                                id: label

                                anchors.verticalCenter: parent.verticalCenter

                                text: title
                            }
                        }

                        Loader {
                            id: control

                            Layout.preferredWidth: root.smallDropdownWidth

                            sourceComponent: {
                                switch (type) {
                                case ExportOptionType.TypeEnum:   return enumComp
                                case ExportOptionType.TypeBool:   return boolComp
                                case ExportOptionType.TypeRange:  return rangeComp
                                case ExportOptionType.TypeString: return strComp
                                }
                            }

                            onLoaded: if (item) item.option = option
                        }
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

    // dynamic export controls

    Component {
        id: enumComp
        StyledDropdown {
            property var option

            model: option.names
            currentIndex: option.value

            onActivated: function(index, value) {
                dynamicOptionsModel.setData(dynamicOptionsModel.index(option.index, 0),
                                            option.values[index],
                                            ExportOptionType.ValueRole)
            }
        }
    }

    Component {
        id: boolComp
        CheckBox {
            property var option

            checked: Boolean(option.value)
            onClicked: dynamicOptionsModel.setData(
                           dynamicOptionsModel.index(option.index, 0),
                           checked,
                           ExportOptionType.ValueRole)
        }
    }

    Component {
        id: rangeComp

        Loader {
            property var option

            sourceComponent: (option.max - option.min < 20) ? sliderComp : spinComp

            onLoaded: if (item) item.option = option
        }
    }

    Component {
        id: sliderComp
        // TODO: test it when Ogg/Opus is supported
        // TODO: add tooltip
        StyledSlider {
            property var option

            from: option.min; to: option.max; stepSize: 1
            value: Number(dynamicOptionsModel.data(dynamicOptionsModel.index(option.index,0),
                                           ExportOptionType.ValueRole))
            onValueChanged: dynamicOptionsModel.setData(
                                dynamicOptionsModel.index(option.index,0),
                                Math.round(option.value),
                                ExportOptionType.ValueRole)
            Layout.minimumWidth: 180
        }
    }

    Component {
        id: spinComp

        IncrementalPropertyControl {
            property var option

            decimals: 0
            step: 1
            minValue: option.min
            maxValue: option.max
            currentValue: option.value
            onValueEdited: function(newValue) {
                dynamicOptionsModel.setData(
                    dynamicOptionsModel.index(option.index,0),
                    newValue,
                    ExportOptionType.ValueRole)
            }
        }
    }

    Component {
        id: strComp
        // StyledTextLabel for external program export description
        StyledTextLabel {
            property var option

            // TODO
        }
    }
}
