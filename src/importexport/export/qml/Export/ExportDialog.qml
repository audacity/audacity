/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.UiComponents 1.0
import Audacity.Export 1.0

import "internal"

StyledDialogView {
    id: root

    title: qsTrc("export", "Export audio")

    contentWidth: 612
    contentHeight: 600

    margins: 12

    modal: true
    alwaysOnTop: true

    property int dropdownWidth: 358
    property int smallDropdownWidth: 364 * 0.65
    property int labelColumnWidth: 140

    onNavigationActivateRequested: {
        typeDropdown.navigation.requestActive()
    }

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
        exportPreferencesModel.init()
        ffmpegPrefModel.init()
        dynamicOptionsModel.init()
    }

    ColumnLayout {
        id: mainColumn

        spacing: 12

        onImplicitHeightChanged: {
            Qt.callLater(function() {
                root.contentHeight = implicitHeight
            })
        }

        ColumnLayout {
            Layout.margins: 4
            spacing: 16

            BaseSection {
                id: typeSection

                title: qsTrc("export", "Export")

                rowSpacing: 8

                navigation.section: root.navigationSection
                navigation.order: 1

                RowLayout {

                    Item {
                        width: root.labelColumnWidth

                        StyledTextLabel {
                            id: typeLabel

                            text: qsTrc("export", "Type")

                            anchors.verticalCenter: parent.verticalCenter
                        }
                    }

                    StyledDropdown {
                        id: typeDropdown

                        Layout.preferredWidth: root.dropdownWidth

                        textRole: "name"
                        valueRole: "code"

                        popupItemsCount: 11
                        currentIndex: indexOfValue(exportPreferencesModel.currentProcess)
                        model: exportPreferencesModel.processList


                        navigation.name: "TypeDropdown"
                        navigation.panel: typeSection.navigation
                        navigation.order: 1
                        navigation.accessible.name: typeLabel.text + ": " + currentText

                        indeterminateText: ""

                        onActivated: function(index, value) {
                            exportPreferencesModel.setCurrentProcess(value)
                        }
                    }
                }
            }

            SeparatorLine {}

            BaseSection {
                id: fileSection

                title: qsTrc("export", "File")

                rowSpacing: 8

                navigation.section: root.navigationSection
                navigation.order: typeSection.navigation.order + 1

                RowLayout {

                    Item {
                        width: root.labelColumnWidth
                        StyledTextLabel {
                            id: filenameLabel

                            text: qsTrc("export", "File name")

                            anchors.verticalCenter: parent.verticalCenter
                        }
                    }

                    TextInputField {
                        id: filenameField

                        Layout.fillWidth: true
                        Layout.minimumWidth: implicitWidth

                        currentText: exportPreferencesModel.filename

                        implicitWidth: root.dropdownWidth

                        navigation.name: "FileNameFieldBox"
                        navigation.panel: fileSection.navigation
                        navigation.order: 1
                        navigation.accessible.name: filenameLabel.text + ": " + currentText

                        onTextChanged: function(newTextValue) {
                            exportPreferencesModel.setFilename(newTextValue)
                        }
                    }
                }

                RowLayout {

                    Item {
                        width: root.labelColumnWidth
                        StyledTextLabel {
                            id: folderLabel

                            text: qsTrc("export", "Folder")

                            anchors.verticalCenter: parent.verticalCenter
                        }
                    }

                    FilePicker {
                        id: dirPicker

                        pickerType: FilePicker.PickerType.Any
                        pathFieldWidth: root.dropdownWidth
                        spacing: 8
                        filter: exportPreferencesModel.fileFilter()

                        buttonType: FlatButton.Horizontal
                        orientation: Qt.Horizontal

                        path: exportPreferencesModel.directoryPath
                        dir: exportPreferencesModel.directoryPath

                        navigation: fileSection.navigation
                        navigationRowOrderStart: filenameField.navigation.order + 1

                        onPathEdited: function(newPath) {
                            exportPreferencesModel.setFilePickerPath(newPath)
                        }
                    }
                }

                RowLayout {

                    Item {
                        width: root.labelColumnWidth
                        StyledTextLabel {
                            id: formatLabel

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

                        navigation.name: "FormatDropdown"
                        navigation.panel: fileSection.navigation
                        navigation.order: dirPicker.navigationRowOrderStart + 2
                        navigation.accessible.name: formatLabel.text + ": " + currentText

                        indeterminateText: ""

                        onActivated: function(index, value) {
                            exportPreferencesModel.setCurrentFormat(value)
                        }
                    }
                }
            }

            SeparatorLine {}

            BaseSection {
                id: audioSection

                title: qsTrc("export", "Audio options")

                rowSpacing: 8

                navigation.section: root.navigationSection
                navigation.order: fileSection.navigation.order + 1

                RowLayout {

                    Item {
                        id: spacer

                        width: root.labelColumnWidth

                        StyledTextLabel {
                            id: channelsLabel

                            text: qsTrc("export", "Channels")

                            anchors.verticalCenter: parent.verticalCenter
                        }
                    }

                    RadioButtonGroup {
                        id: channelsGroup

                        orientation: Qt.Horizontal

                        Layout.preferredWidth: root.dropdownWidth
                        Layout.preferredHeight: 20

                        property int navigationOrderStart: 0

                        Row {
                            width: parent.width
                            spacing: 10

                            RoundedRadioButton {
                                id: monoBtn

                                checked: exportPreferencesModel.exportChannels == ExportChannels.MONO
                                enabled: exportPreferencesModel.maxExportChannels > 0
                                text: qsTrc("export", "Mono")

                                spacing: 8

                                navigation.name: "MonoBox"
                                navigation.panel: audioSection.navigation
                                navigation.order: channelsGroup.navigationOrderStart

                                onToggled: {
                                    exportPreferencesModel.setExportChannels(ExportChannels.MONO)
                                }
                            }

                            RoundedRadioButton {
                                id: stereoBtn

                                checked: exportPreferencesModel.exportChannels == ExportChannels.STEREO
                                enabled: exportPreferencesModel.maxExportChannels > 1
                                text: qsTrc("export", "Stereo")

                                spacing: 8

                                navigation.name: "StereoBox"
                                navigation.panel: audioSection.navigation
                                navigation.order: monoBtn.navigation.order + 1

                                onToggled: {
                                    exportPreferencesModel.setExportChannels(ExportChannels.STEREO)
                                }
                            }

                            RoundedRadioButton {
                                id: customBtn

                                checked: exportPreferencesModel.exportChannels == ExportChannels.CUSTOM
                                text: qsTrc("export", "Custom mapping")
                                enabled: false // until custom mapping grid is implemented

                                spacing: 8

                                navigation.name: "CustomBox"
                                navigation.panel: audioSection.navigation
                                navigation.order: stereoBtn.navigation.order + 1

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
                            id: sampleRateLabel

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

                        navigation.name: "SampleRateDropdown"
                        navigation.panel: audioSection.navigation
                        navigation.order: customBtn.navigation.order + 1
                        navigation.accessible.name: formatLabel.text + ": " + currentText

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

                            navigation.name: "CustomFFmpegButton"
                            navigation.panel: audioSection.navigation
                            navigation.order: sampleRateDropdown.navigation.order + 1

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

            SeparatorLine {}

            BaseSection {
                id: renderingSection
                title: qsTrc("export", "Rendering")

                navigation.section: root.navigationSection
                navigation.order: audioSection.navigation.order + 1

                ColumnLayout {
                    CheckBox {
                        id: trimBlankSpaceCheckBox
                        width: parent.width

                        text: qsTrc("export", "Trim blank space before first clip")
                        enabled: false

                        navigation.name: "TrimBlankSpaceBox"
                        navigation.panel: renderingSection.navigation
                        navigation.order: 1
                        navigation.accessible.name: text

                        onClicked: {}
                    }
                }
            }
        }

        SeparatorLine {
            Layout.margins: -root.margins
            width: root.contentWidth + 2 * root.margins
        }

        ButtonBox {
            id: buttonBox

            Layout.fillWidth: true

            spacing: 8

            navigationPanel.section: root.navigationSection
            navigationPanel.order: audioSection.navigation.order + 2

            FlatButton {
                text: qsTrc("appshell/preferences", "Edit metadata")
                buttonRole: ButtonBoxModel.CustomRole
                buttonId: ButtonBoxModel.CustomButton + 1
                isLeftSide: true
                enabled: exportPreferencesModel.hasMetadata

                navigation.panel: buttonBox.navigationPanel
                navigation.order: 1

                onClicked: {
                    exportPreferencesModel.openMetadataDialog()
                }
            }

            FlatButton {
                id: cancelBtn
                text: qsTrc("global", "Cancel")
                buttonRole: ButtonBoxModel.RejectRole
                buttonId: ButtonBoxModel.Cancel
                minWidth: 80

                navigation.panel: buttonBox.navigationPanel
                navigation.order: 2

                onClicked: {
                    exportPreferencesModel.cancel()
                    exportPreferencesModel.setExportSampleRate("")
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

                navigation.panel: buttonBox.navigationPanel
                navigation.order: 3

                onClicked: {
                    if (exportPreferencesModel.verifyExportPossible()) {
                        exportPreferencesModel.apply()
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

            navigation.panel: audioSection.navigation
            navigation.order: sampleRateDropdown.navigation.order + 1 + option.index

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

            navigation.panel: audioSection.navigation
            navigation.order: sampleRateDropdown.navigation.order + 1 + option.index

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

            navigation.panel: audioSection.navigation
            navigation.order: sampleRateDropdown.navigation.order + 1 + option.index

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
