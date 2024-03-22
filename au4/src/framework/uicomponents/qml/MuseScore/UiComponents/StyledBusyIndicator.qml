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

BusyIndicator {
    id: root

    padding: 0

    contentItem: Rectangle {
        id: backgroundCircle
        implicitWidth: 20
        implicitHeight: 20

        width: 20
        height: width

        anchors.centerIn: parent

        radius: width / 2

        color: "transparent"
        border.color: ui.theme.strokeColor
        border.width: width / 10

        RotationAnimator {
            target: foregroundCircle
            running: root.visible && root.running
            from: 0
            to: 360
            loops: Animation.Infinite
            duration: 1000
        }

        Canvas {
            id: foregroundCircle
            anchors.fill: parent

            onPaint: {
                var context = getContext("2d");
                context.arc(width / 2, height / 2,
                            width / 2 - backgroundCircle.border.width / 2,
                            Math.PI / -4, Math.PI / 4);
                context.strokeStyle = ui.theme.accentColor;
                context.lineWidth = backgroundCircle.border.width;
                context.stroke();
            }
        }
    }
}
