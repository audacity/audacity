/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * A Digital Audio Editor
 *
 * Copyright (C) 2025 Audacity BVBA and others
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

    navigation.direction: NavigationPanel.Horizontal

    property alias numberFormats: dropdown.model
    property string currentNumberFormatCode: ""

    signal numberFormatSelected(string formatCode)

    Column {
        spacing: 12

        StyledTextLabel {
            text: qsTrc("appshell/preferences", "Number format")
        }

        StyledDropdown {
            id: dropdown

            width: root.columnWidth

            textRole: "name"
            valueRole: "code"

            currentIndex: dropdown.indexOfValue(root.currentNumberFormatCode)

            navigation.name: "NumberFormatBox"
            navigation.panel: root.navigation
            navigation.column: 1

            indeterminateText: ""

            onActivated: function(index, value) {
                root.numberFormatSelected(value)
            }
        }

        StyledTextLabel {
            text: qsTrc("appshell/preferences", "Example: 1,000,000.99")
            color: ui.theme.fontSecondaryColor
        }
    }
}
