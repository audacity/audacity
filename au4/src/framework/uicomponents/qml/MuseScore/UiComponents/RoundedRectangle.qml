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

Canvas {
    id: root

    property real radius: 0

    property real topLeftRadius: radius
    property real topRightRadius: radius
    property real bottomLeftRadius: radius
    property real bottomRightRadius: radius

    property color color: "#FFFFFF"

    property Border border: Border {}

    onColorChanged: { requestPaint() }
    onBorderChanged: { requestPaint() }

    Connections {
        target: border
        function onColorChanged() { requestPaint() }
        function onWidthChanged() { requestPaint() }
    }

    onTopLeftRadiusChanged: { requestPaint() }
    onTopRightRadiusChanged: { requestPaint() }
    onBottomRightRadiusChanged: { requestPaint() }
    onBottomLeftRadiusChanged: { requestPaint() }

    onOpacityChanged: { requestPaint() }
    onActiveFocusChanged: { requestPaint() }

    onPaint: {
        roundRect(0, 0, width, height)
    }

    function roundRect(x1, y1, x2, y2)
    {
        const context = root.getContext("2d")

        context.clearRect(0, 0, width, height)

        let needsFill = color.a > 0
        let needsStroke = border.width > 0 && border.color.a > 0

        if (!needsFill && !needsStroke) {
            return
        }

        context.clearRect(0, 0, width, height)

        context.beginPath()
        context.fillStyle = root.color
        context.strokeStyle = root.border.color
        context.lineWidth = root.border.width

        let b = border.width / 2
        let x1Inner = x1 + b
        let y1Inner = y1 + b
        let x2Inner = x2 - b
        let y2Inner = y2 - b

        context.moveTo(x1 + topLeftRadius, y1Inner)

        if (topRightRadius == 0) {
            context.lineTo(x2Inner, y1Inner)
        } else {
            context.lineTo(x2 - topRightRadius, y1Inner)
            context.quadraticCurveTo(x2Inner, y1Inner, x2Inner, y1 + topRightRadius)
        }

        if (bottomRightRadius == 0) {
            context.lineTo(x2Inner, y2Inner)
        } else {
            context.lineTo(x2Inner, y2 - bottomRightRadius)
            context.quadraticCurveTo(x2Inner, y2Inner, x2 - bottomRightRadius, y2Inner)
        }

        if (bottomLeftRadius == 0) {
            context.lineTo(x1Inner, y2Inner)
        } else {
            context.lineTo(x1 + bottomLeftRadius, y2Inner)
            context.quadraticCurveTo(x1Inner, y2Inner, x1Inner, y2 - bottomLeftRadius)
        }

        if (topLeftRadius == 0) {
            context.lineTo(x1Inner, y1Inner)
        } else {
            context.lineTo(x1Inner, y1 + topLeftRadius)
            context.quadraticCurveTo(x1Inner, y1Inner, x1 + topLeftRadius, y1Inner)
        }

        if (needsFill) {
            context.fill()
        }

        if (needsStroke) {
            context.stroke()
        }
    }
}
