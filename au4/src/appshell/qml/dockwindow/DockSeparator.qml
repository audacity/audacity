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

Rectangle {
    id: root

    anchors.fill: parent

    color: ui.theme.strokeColor

    readonly property QtObject separatorCpp: parent

    visible: separatorCpp && separatorCpp.isSeparatorVisible

    MouseArea {
        anchors.fill: parent
        anchors.margins: -5 //! NOTE: extra space for user convenience

        cursorShape: (separatorCpp && separatorCpp.showResizeCursor) ? (separatorCpp.isVertical ? Qt.SizeVerCursor : Qt.SizeHorCursor)
                                   : Qt.ArrowCursor
        onPressed: {
            separatorCpp.onMousePressed()
        }

        onReleased: {
            separatorCpp.onMouseReleased()
        }

        onPositionChanged: function(mouse) {
            separatorCpp.onMouseMoved(Qt.point(mouse.x, mouse.y))
        }

        onDoubleClicked: {
            separatorCpp.onMouseDoubleClicked()
        }
    }
}
