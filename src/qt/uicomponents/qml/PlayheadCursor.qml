import QtQuick
import QtQuick.Controls

import Audacity.UiComponents
import Audacity.UiThemes

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
      color: UiTheme.fontColor2

      border {
         color: UiTheme.timecodeColor
         width: 1
      }
   }

   PlayheadCursorHead {
      id: head
   }
}
