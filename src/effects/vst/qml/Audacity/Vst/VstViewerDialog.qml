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
import Muse.Ui 1.0
import Muse.UiComponents 1.0    

StyledDialogView {
    id: root

    property alias instanceId: viewer.instanceId

    title: viewer.title

    contentWidth: Math.max(viewer.implicitWidth, 600)
    contentHeight: viewer.implicitHeight + bbox.implicitHeight + 16

    alwaysOnTop: true

    VstViewer {
        id: viewer
        width: root.contentWidth
        height: implicitHeight
    }

    ButtonBox {
        id: bbox
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        anchors.margins: 8

        //! TODO Move function to ButtonBox (Muse framework)
        function buttonById(id) {
            for (var i = 0; i < bbox.count; i++) {
                var btn = bbox.itemAt(i)
                if (btn.buttonId === id) {
                    return btn
                }
            }

            return null
        }

        Component.onCompleted: {
            // bbox.buttonById(ButtonBoxModel.Apply).enabled = false
        }

        FlatButton {
            id: manageBtn
            text: qsTrc("effects", "Manage")
            buttonRole: ButtonBoxModel.CustomRole
            buttonId: ButtonBoxModel.CustomButton + 1
            isLeftSide: true
            onClicked: viewer.manage(manageBtn)
        }

        FlatButton {
            id: previewBtn
            text: qsTrc("effects", "Preview")
            buttonRole: ButtonBoxModel.CustomRole
            buttonId: ButtonBoxModel.CustomButton + 2
            isLeftSide: true
            onClicked: viewer.preview()
        }

        FlatButton {
            text: qsTrc("global", "Cancel")
            buttonRole: ButtonBoxModel.RejectRole
            buttonId: ButtonBoxModel.Cancel
            onClicked: root.reject()
        }

        FlatButton {
            text: qsTrc("global", "Apply")
            buttonRole: ButtonBoxModel.AcceptRole
            buttonId: ButtonBoxModel.Apply
            accentButton: true
            onClicked: root.accept()
        }
    }
}
