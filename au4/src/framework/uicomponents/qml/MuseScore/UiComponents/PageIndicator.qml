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
import QtQuick.Controls 2.15 as Controls

Controls.PageIndicator {
    id: root

    padding: 0
    spacing: 6

    delegate: Rectangle {
        implicitWidth: 6
        implicitHeight: implicitWidth

        radius: width / 2

        color: ui.theme.fontPrimaryColor
        opacity: index === root.currentIndex ? 0.7 : 0.2

        Behavior on opacity {
            OpacityAnimator {
                duration: 50
            }
        }
    }
}
