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

Rectangle {
    id: root

    property bool highlight: false
    property var mouseArea: null

    color: "transparent"

    border.width: ui.theme.borderWidth
    border.color: ui.theme.strokeColor

    Rectangle {
        id: focusBorder

        anchors.fill: parent

        visible: root.highlight

        color: "transparent"

        border.color: ui.theme.fontPrimaryColor
        border.width: ui.theme.navCtrlBorderWidth
        radius: Number(parent.radius) > 0 ? parent.radius : 0
    }

    states: [
        State {
            name: "PRESSED"
            when: root.mouseArea.pressed

            PropertyChanges {
                target: root
                color: ui.theme.buttonColor
                opacity: 1
            }
        },

        State {
            name: "HOVERED"
            when: root.mouseArea.containsMouse && !root.mouseArea.pressed

            PropertyChanges {
                target: root
                color: ui.theme.buttonColor
                opacity: 0.5
            }
        }
    ]
}
