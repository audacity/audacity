/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
import QtQuick 2.15
import QtQuick.Layouts 1.15

import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0

Item {
    id: root

    enum PickerType {
        File,
        Directory,
        MultipleDirectories
    }
    property int pickerType: FilePicker.PickerType.File

    property alias path: pathField.currentText

    property alias dialogTitle: filePickerModel.title
    property alias filter: filePickerModel.filter
    property alias dir: filePickerModel.dir

    property NavigationPanel navigation: null
    property int navigationRowOrderStart: 0
    property int navigationColumnOrderStart: 0

    property string pathFieldTitle: qsTrc("ui", "Current path:")

    property alias pathFieldWidth: pathField.implicitWidth
    property alias spacing: row.spacing

    signal pathEdited(var newPath)

    implicitWidth: row.implicitWidth
    implicitHeight: row.implicitHeight

    FilePickerModel {
        id: filePickerModel
    }

    RowLayout {
        id: row
        anchors.fill: parent
        spacing: 12

        TextInputField {
            id: pathField
            Layout.fillWidth: true
            Layout.minimumWidth: implicitWidth
            Layout.alignment: Qt.AlignVCenter

            implicitWidth: 0

            navigation.name: "PathFieldBox"
            navigation.panel: root.navigation
            navigation.row: root.navigationRowOrderStart
            navigation.enabled: root.visible && root.enabled
            navigation.column: root.navigationColumnOrderStart
            navigation.accessible.name: root.pathFieldTitle + " " + pathField.currentText

            onTextChanged: function(newTextValue) {
                root.pathEdited(newTextValue)
            }
        }

        FlatButton {
            id: button
            Layout.alignment: Qt.AlignVCenter
            icon: IconCode.OPEN_FILE

            navigation.name: "FilePickerButton"
            navigation.panel: root.navigation
            navigation.row: root.navigationRowOrderStart
            navigation.enabled: root.visible && root.enabled
            navigation.column: root.navigationColumnOrderStart + 1
            accessible.name: root.pickerType === FilePicker.PickerType.File ? qsTrc("ui", "Choose file")
                                                                            : qsTrc("ui", "Choose directory")

            onClicked: {
                switch (pickerType) {
                case FilePicker.PickerType.File: {
                    var selectedFile = filePickerModel.selectFile()
                    if (Boolean(selectedFile)) {
                        root.pathEdited(selectedFile)
                    }

                    break
                }
                case FilePicker.PickerType.Directory: {
                    var selectedDirectory = filePickerModel.selectDirectory()
                    if (Boolean(selectedDirectory)) {
                        root.pathEdited(selectedDirectory)
                    }

                    break
                }
                case FilePicker.PickerType.MultipleDirectories:{
                    var selectedDirectories = filePickerModel.selectMultipleDirectories(root.path)
                    root.pathEdited(selectedDirectories)
                    break
                }
                }
            }
        }
    }
}
