import QtQuick
import QtQuick.Controls

import Audacity
import Audacity.UiComponents

Rectangle {
   id: root
   width: 111
   height: 28
   radius: 3
   color: appConfig.timecodeColor
   objectName: "TimeControl"

   property int value: 0

   function displayValue(value) {
      var text = String(value).padStart(6, '0')
      return text.slice(0, 3) + ',' + text.slice(3)
   }

   function start() {
      timer.start()
   }

   function stop() {
      timer.stop()
   }

   function reset() {
      timer.stop()
      value = 0
   }

   Timer {
      id: timer
      interval: 1000
      repeat: true

      onTriggered: {
         value += 1
      }
   }

   Rectangle {
      id: separator
      x: root.width - downArrow.width - 1
      width: 1
      height: root.height
      color: appConfig.backgroundColor1
   }

   Text {
      id: seconds
      x: 8
      width: 70
      height: 16
      anchors.verticalCenter: parent.verticalCenter
      horizontalAlignment: Text.AlignRight
      color: appConfig.fontColor2

      text: displayValue(value)

      font {
         family: appConfig.bodyFont.family
         pixelSize: 16
      }
   }

   Text {
      id: secondsSuffix
      text: "s"
      x: seconds.x + seconds.width
      width: 9
      height: 16
      anchors.verticalCenter: parent.verticalCenter
      color: appConfig.fontColor2
      opacity: 0.7

      font {
         family: appConfig.bodyFont.family
         pixelSize: 16
      }
   }

   Text {
      id: downArrow
      x: root.width - width
      width: 16
      anchors.verticalCenter: parent.verticalCenter
      color: appConfig.fontColor2
      text: String.fromCharCode(IconCode.SMALL_ARROW_DOWN)

      font {
         family: appConfig.iconFont.family
         pixelSize: 16
      }
   }
}
