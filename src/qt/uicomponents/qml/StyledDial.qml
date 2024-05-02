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
import QtQuick
import QtQuick.Controls

import Audacity.Ui

Dial {
   id: root
   implicitWidth: 30
   implicitHeight: implicitWidth
   width: implicitWidth
   height: width
   objectName: "StyledDial"

   from: -1.0
   to: 1.0
   value: 0

   QtObject {
      id: prv

      readonly property real outerArcLineWidth: 3
      readonly property real innerArcLineWidth: 2
      readonly property real dialOffset: outerArcLineWidth + innerArcLineWidth

      readonly property color valueArcColor: ui.theme.brandColor
      readonly property color outerArcColor: colorWithAlpha(ui.theme.strokeColor1, ui.theme.opacityLight)
      readonly property color innerArcColor: ui.theme.strokeColor1
      readonly property color dialNeedleColor: ui.theme.strokeColor3
      readonly property color dialBackgroundColor: ui.theme.backgroundColor1

      property int initialValue: 0
      property real dragStartX: 0
      property real dragStartY: 0

      function colorWithAlpha(color, alpha) {
         return Qt.rgba(color.r, color.g, color.b, alpha)
      }

      onValueArcColorChanged: canvas.requestPaint()
      onOuterArcColorChanged: canvas.requestPaint()
      onInnerArcColorChanged: canvas.requestPaint()
      onDialNeedleColorChanged: canvas.requestPaint()
      onDialBackgroundColorChanged: canvas.requestPaint()
   }

   onAngleChanged: canvas.requestPaint()
   onValueChanged: canvas.requestPaint()

   background: Canvas {
      id: canvas
      anchors.fill: parent
      antialiasing: true

      onPaint: {
         var ctx = canvas.context
         if (!ctx) {
            ctx = getContext("2d")
            ctx.lineCap = "round"
         }

         ctx.clearRect(0, 0, canvasSize.width, canvasSize.height)

         // Dial background
         ctx.fillStyle = prv.dialBackgroundColor
         ctx.beginPath()
         ctx.ellipse(prv.dialOffset, prv.dialOffset, canvasSize.width - 2 * prv.dialOffset, canvasSize.height - 2 * prv.dialOffset)
         ctx.fill()

         // Gauge background
         ctx.lineWidth = prv.outerArcLineWidth
         ctx.strokeStyle = prv.outerArcColor
         ctx.beginPath()
         ctx.arc(width/2, height/2, root.width/2 - prv.outerArcLineWidth/2, -140 * (Math.PI/180) - Math.PI/2, 140 * (Math.PI/180) - Math.PI/2, false)
         ctx.stroke()

         // Knob amount
         ctx.strokeStyle = prv.valueArcColor
         ctx.beginPath()
         ctx.arc(width/2, height/2, root.width/2 - prv.outerArcLineWidth/2, -Math.PI/2, root.angle * (Math.PI/180) - Math.PI/2, root.angle < 0)
         ctx.stroke()

         // Dial stroke
         ctx.lineWidth = prv.innerArcLineWidth
         ctx.strokeStyle = prv.innerArcColor
         ctx.beginPath()
         ctx.arc(width/2, height/2, root.width/2 - (prv.outerArcLineWidth + prv.innerArcLineWidth/2), 0, Math.PI * 2, false)
         ctx.stroke()
      }
   }

   // Dial needle
   handle: Rectangle {
      x: root.width/2 - width/2
      y: prv.outerArcLineWidth + prv.innerArcLineWidth * 2

      height: root.width/2 - prv.outerArcLineWidth - prv.innerArcLineWidth * 3
      width: 2

      color: prv.dialNeedleColor
      antialiasing: true

      transformOrigin: Item.Bottom
      rotation: root.angle
   }

   MouseArea {
      id: mouseArea
      anchors.fill: parent
      preventStealing: true

      onDoubleClicked: {
         value = 0
      }

      onPressed: function(mouse) {
         prv.initialValue = root.value
         prv.dragStartX = mouse.x
         prv.dragStartY = mouse.y
      }

      onPositionChanged: function(mouse) {
         let dx = mouse.x - prv.dragStartX
         let dy = mouse.y - prv.dragStartY
         let dist = Math.sqrt(dx * dx + dy * dy)
         let sgn = (dy < dx) ? 1 : -1
         let newValue = (prv.initialValue + dist * sgn) / 100.0

         root.value = Math.max(root.from, Math.min(newValue, root.to))
      }
   }
}
