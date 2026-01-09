/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * A Digital Audio Editor
 *
 * Copyright (C) 2026 Audacity BVBA and others
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
import Muse.UiComponents
import Muse.GraphicalEffects

Rectangle {
    id: root

    property string source: ""
    property color unaccentedBorderColor: "transparent"

    signal clicked

    layer.enabled: true
    layer.effect: RoundedCornersEffect {
        radius: root.radius
    }

    AnimatedImage {
        source: root.source
        playing: true

        width: root.width
        height: root.height
    }

    RoundedRectangle {
        anchors.fill: parent

        color: "transparent"
        border.color: mouseArea.containsMouse ? ui.theme.accentColor : root.unaccentedBorderColor
        border.width: 1
        radius: root.radius
    }

    MouseArea {
        id: mouseArea

        anchors.fill: parent

        hoverEnabled: true

        onClicked: {
            root.clicked()
        }
    }
}
