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
import QtQuick 2.9

import Muse.Ui 1.0
import Muse.UiComponents 1.0

Column {
    id: root

    property string title: ""
    property alias info: textField.hint

    property alias navigation: textField.navigation

    spacing: 10

    StyledTextLabel {
        anchors.left: parent.left
        anchors.right: parent.right

        font: ui.theme.bodyBoldFont
        horizontalAlignment: Text.AlignLeft
        text: title
    }

    TextInputField {
        id: textField

        navigation.accessible.name: root.title + " " + currentText

        onTextChanged: function(newTextValue) {
            root.info = newTextValue
        }
    }
}

