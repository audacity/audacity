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
import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0


Rectangle {
    id: root

    property alias keynavSection: keynavsec
    property alias sectionName: keynavsec.name
    property alias sectionOrder: keynavsec.order
    property alias active: keynavsec.active

    default property alias content: contentItem.data

    NavigationFocusBorder { navigationCtrl: keynavsec }

    NavigationSection {
        id: keynavsec
        onActiveChanged: {
            console.debug("KeyNavSection.qml active: " + keynavsec.active)
            if (keynavsec.active) {
                root.forceActiveFocus()

            }
        }
    }

    Item {
        id: contentItem
        anchors.fill: parent
    }

    FlatButton {
        id: btn
        anchors.right: parent.right
        width: 20
        height: 20
        text: keynavsec.enabled ? "ON" : "OFF"
        onClicked: keynavsec.enabled = !keynavsec.enabled
    }
}
