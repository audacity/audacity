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

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Character set used when importing binary files")

    property var charsets: null
    property string currentOvertureCharset: ""

    signal overtureCharsetChangeRequested(string charset)

    ComboBoxWithTitle {
        title: qsTrc("appshell/preferences", "Overture import character set:")
        columnWidth: root.columnWidth

        currentIndex: control.indexOfValue(root.currentOvertureCharset)
        model: root.charsets

        navigation.name: "OvertureBox"
        navigation.panel: root.navigation
        navigation.row: 1

        onValueEdited: function(newIndex, newValue) {
            root.overtureCharsetChangeRequested(newValue)
        }
    }
}
