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

ScrollBar {
    id: root

    property alias color: handle.color
    property alias border: handle.border

    property real thickness: 8
    property real minimumSizeInPixels: 30

    readonly property bool isVertical: orientation === Qt.Vertical
    readonly property bool isScrollbarNeeded: size > 0.0 && size < 1.0

    visible: isScrollbarNeeded
    padding: 4
    minimumSize: minimumSizeInPixels / (isVertical ? height : width)

    contentItem: Rectangle {
        id: handle

        implicitWidth: root.thickness
        implicitHeight: root.thickness

        radius: root.thickness / 2
        color: ui.theme.fontPrimaryColor
        opacity: 0.0
        visible: false

        states: [
            State {
                name: "active"
                when: root.policy === ScrollBar.AlwaysOn || (root.active && root.isScrollbarNeeded)

                PropertyChanges {
                    target: handle
                    visible: true
                    opacity: root.pressed ? 0.7 : 0.3
                }
            }
        ]

        transitions: Transition {
            from: "active"

            SequentialAnimation {
                PauseAnimation { duration: 200 }

                NumberAnimation {
                    target: handle
                    property: "opacity"
                    duration: 100
                    to: 0.0
                }

                PropertyAction {
                    target: handle
                    property: "visible"
                }
            }
        }
    }

    function activate() {
        root.active = true
        Qt.callLater(function() {
            root.active = Qt.binding( function() { return root.hovered || root.pressed } )
        })
    }
}
