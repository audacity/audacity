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

import Muse.UiComponents 1.0

Row {
    id: root

    property alias title: titleLabel.text

    property real columnWidth: 208

    property alias currentIndex: comboBox.currentIndex
    property alias currentValue: comboBox.currentValue
    property alias model: comboBox.model
    property alias control: comboBox

    property alias navigation: comboBox.navigation

    signal valueEdited(int newIndex, var newValue)

    spacing: 12

    function indexOfValue(value) {
        return comboBox.indexOfValue(value)
    }

    StyledTextLabel {
        id: titleLabel

        width: root.columnWidth
        anchors.verticalCenter: parent.verticalCenter

        horizontalAlignment: Qt.AlignLeft
        wrapMode: Text.WordWrap
        maximumLineCount: 2
    }

    StyledDropdown {
        id: comboBox

        width: root.columnWidth

        navigation.accessible.name: root.title + " " + currentText

        indeterminateText: ""

        onActivated: function(index, value) {
            root.valueEdited(index, value)
        }
    }
}
