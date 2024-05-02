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

import Muse.Ui 1.0
import Muse.UiComponents 1.0

Rectangle {
    id: root

    color: ui.theme.backgroundPrimaryColor

    property alias navigation: buttonBox.navigationPanel

    signal revertFactorySettingsRequested()
    signal applyRequested()
    signal rejectRequested()

    ButtonBox {
        id: buttonBox

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.verticalCenter: parent.verticalCenter
        anchors.margins: 20

        buttons: [ ButtonBoxModel.Cancel, ButtonBoxModel.Ok ]

        FlatButton {
            text: qsTrc("appshell/preferences", "Reset preferences")
            buttonRole: ButtonBoxModel.CustomRole
            buttonId: ButtonBoxModel.CustomButton + 1
            isLeftSide: true

            onClicked: {
                root.revertFactorySettingsRequested()
            }
        }

        onStandardButtonClicked: function(buttonId) {
            if (buttonId === ButtonBoxModel.Cancel) {
                root.rejectRequested()
            } else if (buttonId === ButtonBoxModel.Ok) {
                root.applyRequested()
            }
        }
    }
}
