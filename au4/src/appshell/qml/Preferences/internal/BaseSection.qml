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

Column {
    id: root

    default property alias contentData: sectionContent.data

    width: parent.width
    spacing: 18

    property alias title: titleLabel.text
    property int columnWidth: 208
    property int columnSpacing: 12
    property int rowSpacing: 12

    property int navigationOrderStart: 0
    property NavigationPanel navigation: NavigationPanel {
        name: root.title
        direction: NavigationPanel.Vertical
        accessible.name: root.title
        enabled: root.enabled && root.visible

        onActiveChanged: function(active) {
            if (active) {
                root.forceActiveFocus()
            }
        }
    }

    StyledTextLabel {
        id: titleLabel
        font: ui.theme.bodyBoldFont
    }

    Column {
        id: sectionContent
        width: parent.width
        spacing: root.rowSpacing
    }
}
