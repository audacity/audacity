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

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Muse.GraphicalEffects

Rectangle {
    id: root

    property string source: ""

    signal clipStyleChangeRequested()

    layer.enabled: true
    layer.effect: EffectOpacityMask {
        maskSource: RoundedRectangle {
            width: root.width
            height: root.height
            radius: root.radius
        }
    }

    Image {
        source: root.source

        width: root.width
        height: root.height
    }

    RoundedRectangle {
        anchors.fill: parent

        color: "transparent"
        border.color: ui.theme.accentColor
        border.width: 1
        radius: root.radius
        visible: mouseArea.containsMouse
    }

    MouseArea {
        id: mouseArea

        anchors.fill: parent

        hoverEnabled: true

        onClicked: {
            root.clipStyleChangeRequested()
        }
    }
}
