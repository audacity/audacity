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

import "internal"

StyledDialogView {
    id: root

    contentWidth: 664
    contentHeight: 300

    property string selectedDirectories: ""
    property string startDir: ""

    SelectMultipleDirectoriesModel {
        id: directoriesModel
    }

    QtObject {
        id: prv

        readonly property int sideMargin: 36
        readonly property int buttonsMargin: 24
    }

    Component.onCompleted: {
        directoriesModel.load(root.startDir, root.selectedDirectories)
    }

    onNavigationActivateRequested: {
        view.focusOnFirst()
    }

    ColumnLayout {
        anchors.fill: parent
        spacing: 0

        DirectoriesTopPanel {
            Layout.fillWidth: true
            Layout.preferredHeight: childrenRect.height
            Layout.topMargin: prv.sideMargin

            sideMargin: prv.sideMargin
            buttonsMargin: prv.buttonsMargin

            isRemovingAvailable: directoriesModel.isRemovingAvailable

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 1

            onAddDirectoryRequested: {
                directoriesModel.addDirectory()
            }

            onRemoveSelectedDirectoriesRequested: {
                directoriesModel.removeSelectedDirectories()
            }
        }

        DirectoriesView {
            id: view

            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.topMargin: 38
            Layout.bottomMargin: 24

            model: directoriesModel

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 2
        }

        ButtonBox {
            id: buttonBox

            Layout.fillWidth: true
            Layout.rightMargin: prv.buttonsMargin
            Layout.bottomMargin: prv.buttonsMargin
            Layout.alignment: Qt.AlignRight | Qt.AlignBottom

            buttons: [ ButtonBoxModel.Cancel, ButtonBoxModel.Ok ]

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 3

            onStandardButtonClicked: function(buttonId) {
                if (buttonId === ButtonBoxModel.Cancel) {
                    root.reject()
                } else if (buttonId === ButtonBoxModel.Ok) {
                    root.ret = { errcode: 0, value: directoriesModel.directories() }
                    root.hide()
                }
            }
        }
    }
}
