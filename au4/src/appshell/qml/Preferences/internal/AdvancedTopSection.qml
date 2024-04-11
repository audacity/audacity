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

BaseSection {
    id: root

    navigation.direction: NavigationPanel.Horizontal

    property alias searchText: searchField.searchText

    signal resetToDefaultRequested()

    RowLayout {
        //! NOTE: Added to prevent components clipping when navigating
        width: root.width - root.padding * 2

        FlatButton {
            Layout.alignment: Qt.AlignVCenter

            text: qsTrc("appshell/preferences", "Reset to default")

            navigation.name: "ResetToDefaultButton"
            navigation.panel: root.navigation
            navigation.column: 0

            onClicked: {
                root.resetToDefaultRequested()
            }
        }

        Item {
            Layout.fillWidth: true
            Layout.fillHeight: true
        }

        SearchField {
            id: searchField

            Layout.alignment: Qt.AlignVCenter
            Layout.preferredWidth: 160

            //: Search advanced preferences
            hint: qsTrc("appshell/preferences", "Search advanced")

            navigation.name: "SearchAdvancedField"
            navigation.panel: root.navigation
            navigation.column: 1
        }
    }
}
