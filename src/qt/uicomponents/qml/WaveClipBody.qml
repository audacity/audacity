import QtQuick
import QtQuick.Controls

import Audacity.UiComponents
import Audacity.Ui

Canvas {
   id: root
   implicitWidth: 398
   implicitHeight: 105
   width: implicitWidth
   height: implicitHeight
   smooth: true
   antialiasing: true
   objectName: "WaveClipBody"

   property int radius: 3
   property color color: ui.theme.backgroundColor2

   function colorWithAlpha(color, alpha) {
      return Qt.rgba(color.r, color.g, color.b, alpha)
   }

   onColorChanged: root.requestPaint()
   onWidthChanged: root.requestPaint()
   onHeightChanged: root.requestPaint()

   onPaint: {
      var ctx = getContext("2d")
      ctx.fillStyle = colorWithAlpha(root.color, ui.theme.opacityMedium)

      ctx.beginPath()
         // top edge
         ctx.lineTo(width, 0)

         // right edge
         ctx.lineTo(width, height - radius)

         // bottom right rounded corner
         ctx.arc(width - radius, height - radius, radius, 0, Math.PI / 2)

         // bottom edge
         ctx.lineTo(radius - 1, height)

         // bottom left rounded corner
         ctx.arc(radius - 1, height - radius, radius, Math.PI / 2, Math.PI)

         // left edge
         ctx.lineTo(0, 0)
      ctx.fill()
   }
}
