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

    title: qsTrc("appshell/preferences", "MIDI")

    property alias shortestNotes: shortestNotesBox.model
    property int currentShortestNote: 0

    signal currentShortestNoteChangeRequested(int note)

    ComboBoxWithTitle {
        id: shortestNotesBox

        title: qsTrc("appshell/preferences", "Shortest note:")
        columnWidth: root.columnWidth

        currentIndex: control.indexOfValue(root.currentShortestNote)

        control.textRole: "title"
        control.valueRole: "value"

        navigation.name: "ShortestNoteBox"
        navigation.panel: root.navigation
        navigation.row: 0

        onValueEdited: function(newIndex, newValue) {
            root.currentShortestNoteChangeRequested(newValue)
        }
    }
}
