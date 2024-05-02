import QtQuick

import Audacity.UiComponents
import Audacity.Ui

Canvas {
   id: root
   implicitWidth: 15
   implicitHeight: 14
   width: implicitWidth
   height: implicitHeight
   antialiasing: true
   smooth: true

   property int radius: 2
   readonly property color strokeColor: ui.theme.backgroundColor4
   readonly property color fillColor: ui.theme.fontColor2

   onStrokeColorChanged: root.requestPaint()
   onFillColorChanged: root.requestPaint()

   onPaint: {
      var ctx = getContext("2d")
      ctx.lineCap = "squared"
      ctx.lineWidth = 1
      ctx.strokeStyle = strokeColor
      ctx.fillStyle = fillColor

      ctx.beginPath()
         // top left rounded corner
         ctx.arc(radius, radius, radius, Math.PI, -Math.PI / 2)

         // top edge
         ctx.lineTo(root.width - radius, 0)

         // top right rounded corner
         ctx.arc(root.width - radius, radius, radius, -Math.PI / 2, 0)

         // right edge
         ctx.lineTo(root.width, root.height - 4 - 1)

         // right sloped corner
         ctx.lineTo(root.width / 2, root.height - 1)

         // left sloped corner
         ctx.lineTo(0, root.height - 4 - 1)

         // left edge
         ctx.lineTo(0, radius)
      ctx.closePath()

      ctx.fill()
      ctx.stroke()
   }
}
