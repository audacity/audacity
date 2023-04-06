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

      readonly property color valueArcColor: appConfig.accentColor
      readonly property color outerArcColor: appConfig.buttonColor
      readonly property color innerArcColor: colorWithAlpha(appConfig.fontColor1, 0.5)
      readonly property color dialNeedleColor: appConfig.fontColor1

      function colorWithAlpha(color, alpha) {
         return Qt.rgba(color.r, color.g, color.b, alpha)
      }
   }

   onAngleChanged: canvas.requestPaint()

   background: Canvas {
      id: canvas
      anchors.fill: parent
      antialiasing: true

      onPaint: {
         var ctx = canvas.context
         if (!ctx) {
            ctx = getContext("2d")
            ctx.linCap = "squared"
         }

         ctx.clearRect(0, 0, canvasSize.width, canvasSize.height)

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
}
