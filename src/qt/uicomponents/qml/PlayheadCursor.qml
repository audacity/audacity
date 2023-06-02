import QtQuick
import QtQuick.Controls

import Audacity.UiComponents

Item {
   id: root
   width: implicitWidth
   height: implicitHeight
   implicitWidth: 15
   implicitHeight: 90
   objectName: "PlayheadCursor"

   Rectangle {
      id: stalk
      anchors.centerIn: root
      width: 3
      height: root.height
      color: appConfig.fontColor2

      border {
         color: appConfig.timecodeColor
         width: 1
      }
   }

   PlayheadCursorHead {
      id: head
   }
}
