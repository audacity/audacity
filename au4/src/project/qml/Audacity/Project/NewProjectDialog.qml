/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * Music Composition & Notation
 *
 * Copyright (C) 2024 Audacity BVBA and others
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

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Audacity.Project 1.0

import "internal/NewProject"

StyledDialogView {
    id: root

    title: qsTrc("project", "New project")

    contentHeight: 600
    contentWidth: 1024
    resizable: true

    objectName: "NewProjectDialog"

    function onDone() {
        var result = {}

        if (newProjectModel.createProject(result)) {
            root.activateParentOnClose = false
            root.accept()
        }
    }

    NewProjectModel {
        id: newProjectModel
    }

    Item {
        id: popupsAnchorItem
        anchors.fill: parent
        anchors.margins: 8
    }

    StackLayout {
        id: pagesStack

        anchors.top: parent.top
        anchors.topMargin: 12
        anchors.left: parent.left
        anchors.leftMargin: 12
        anchors.right: parent.right
        anchors.rightMargin: 12
        anchors.bottom: footer.top
        anchors.bottomMargin: 20

        ProjectInfoPage {
            id: projectInfoPage

            navigationSection: root.navigationSection
            popupsAnchorItem: popupsAnchorItem
        }
    }

    RowLayout {
        id: footer

        anchors.left: parent.left
        anchors.leftMargin: 24
        anchors.right: parent.right
        anchors.rightMargin: 16
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 16

        spacing: 12

        StyledTextLabel {
            id: descriptionLabel
            text: pagesStack.currentIndex === 0
                  ? chooseInstrumentsAndTemplatePage.description
                  : ""

            Layout.fillWidth: true
            Layout.maximumHeight: buttonBox.height

            font: ui.theme.bodyFont
            opacity: 0.7
            horizontalAlignment: Text.AlignLeft
            wrapMode: Text.Wrap
        }

        ButtonBox {
            id: buttonBox

            Layout.preferredWidth: implicitWidth

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 100

            FlatButton {
                text: qsTrc("global", "Cancel")
                buttonRole: ButtonBoxModel.CustomRole
                buttonId: ButtonBoxModel.CustomButton + 1

                onClicked: {
                    root.reject()
                }
            }

            FlatButton {
                text: qsTrc("global", "Back")
                buttonRole: ButtonBoxModel.CustomRole
                buttonId: ButtonBoxModel.CustomButton + 2
                visible: pagesStack.currentIndex > 0

                onClicked: {
                    pagesStack.currentIndex--
                }
            }

            FlatButton {
                text: qsTrc("global", "Next")
                buttonRole: ButtonBoxModel.CustomRole
                buttonId: ButtonBoxModel.CustomButton + 3
                visible: pagesStack.currentIndex < pagesStack.count - 1
                enabled: chooseInstrumentsAndTemplatePage.hasSelection

                onClicked: {
                    pagesStack.currentIndex++
                }
            }

            FlatButton {
                text: qsTrc("global", "Done")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.Done
                accentButton: true

                onClicked: {
                    root.onDone()
                }
            }
        }
    }
}
