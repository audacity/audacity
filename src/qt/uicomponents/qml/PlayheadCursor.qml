import QtQuick
import QtQuick.Controls

import Audacity.UiComponents
import Audacity.Ui

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
      color: ui.theme.fontColor2

      border {
         color: ui.theme.backgroundColor4
         width: 1
      }
   }

   PlayheadCursorHead {
      id: head
   }
}
