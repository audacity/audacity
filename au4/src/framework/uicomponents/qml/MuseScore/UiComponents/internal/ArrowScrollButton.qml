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

import MuseScore.UiComponents 1.0

FlatButton {
    id: root

    property bool isScrollUp: false
    property var view: null

    visible: enabled && (isScrollUp ? (view.contentY !== 0)
                                    : (view.contentY !== view.contentHeight - view.height))

    mouseArea.onContainsMouseChanged: {
        if (!mouseArea.containsMouse) {
            scrollTimer.stop()
            return
        }

        if (scrollTimer.running) {
            return
        }

        scrollTimer.isScrollUp = root.isScrollUp
        scrollTimer.start()
    }

    backgroundItem: Rectangle {
        id: background

        color: ui.theme.backgroundPrimaryColor
        opacity: ui.theme.buttonOpacityNormal

        states: [
            State {
                name: "HOVERED"
                when: root.mouseArea.containsMouse && !root.mouseArea.pressed

                PropertyChanges {
                    target: background
                    opacity: ui.theme.buttonOpacityHover
                }
            }
        ]
    }

    Timer {
        id: scrollTimer

        property bool isScrollUp: false
        readonly property int scrollStep: 10

        repeat: true
        running: false
        interval: 20
        onTriggered: {
            if (isScrollUp) {
                if (root.view.contentY - scrollStep <= 0) {
                    root.view.contentY = 0
                } else {
                    root.view.contentY -= scrollStep
                }
            } else {
                if (root.view.contentY + scrollStep + root.view.height >= root.view.contentHeight) {
                    root.view.contentY = root.view.contentHeight - root.view.height
                } else {
                    root.view.contentY += scrollStep
                }
            }
        }
    }
}
