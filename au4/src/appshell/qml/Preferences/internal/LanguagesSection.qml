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
import QtQuick.Controls 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Languages")

    navigation.direction: NavigationPanel.Horizontal

    property alias languages: dropdown.model
    property string currentLanguageCode: ""
    property bool isNeedRestart: false

    signal languageSelected(string languageCode)
    signal checkForUpdateRequested()

    function setUpdateProgress(current, total, status) {
        progressBtn.to = total
        progressBtn.value = current
        progressBtn.progressStatus = status
    }

    Row {
        spacing: 12

        StyledDropdown {
            id: dropdown

            width: root.columnWidth
            anchors.verticalCenter: parent.verticalCenter

            textRole: "name"
            valueRole: "code"

            popupItemsCount: 11
            currentIndex: dropdown.indexOfValue(root.currentLanguageCode)

            navigation.name: "LanguagesBox"
            navigation.panel: root.navigation
            navigation.column: 1

            indeterminateText: ""

            onActivated: function(index, value) {
                root.languageSelected(value)
            }
        }

        ProgressButton {
            id: progressBtn

            anchors.verticalCenter: parent.verticalCenter

            text: qsTrc("appshell/preferences", "Check for language updates")

            navigationName: "CheckForUpdate"
            navigationPanel: root.navigation
            navigationColumn: 2

            onClicked: {
                root.checkForUpdateRequested()
            }
        }
    }

    StyledTextLabel {
        text: qsTrc("appshell/preferences", "Restart required")
        visible: root.isNeedRestart
    }
}
