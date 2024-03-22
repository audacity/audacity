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

RowLayout {
    id: root

    property alias canEditCurrentShortcut: editButton.enabled
    property alias canClearCurrentShortcuts: clearButton.enabled

    property alias searchText: searchField.searchText

    property int buttonMinWidth: 0

    signal startEditCurrentShortcutRequested()
    signal clearSelectedShortcutsRequested()

    property NavigationPanel navigation: NavigationPanel {
        name: "ShortcutsTopPanel"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        accessible.name: qsTrc("shortcuts", "Shortcuts top panel")

        onActiveChanged: function(active) {
            if (active) {
                root.forceActiveFocus()
            }
        }
    }

    function setSearchText(text) {
        searchField.currentText = text
    }

    FlatButton {
        id: editButton

        minWidth: root.buttonMinWidth

        text: qsTrc("shortcuts", "Define…")

        navigation.name: "DefineShortcutButton"
        navigation.panel: root.navigation
        navigation.column: 0

        onClicked: {
            root.startEditCurrentShortcutRequested()
        }
    }

    FlatButton {
        id: clearButton

        minWidth: root.buttonMinWidth

        text: qsTrc("global", "Clear")

        navigation.name: "ClearShortcutsButton"
        navigation.panel: root.navigation
        navigation.column: 1

        onClicked: {
            root.clearSelectedShortcutsRequested()
        }
    }

    Item { Layout.fillWidth: true }

    SearchField {
        id: searchField

        Layout.preferredWidth: 160

        hint: qsTrc("shortcuts", "Search shortcut")

        navigation.name: "ShortcutSearchField"
        navigation.panel: root.navigation
        navigation.column: 2
    }
}
