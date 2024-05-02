import QtQuick

import Audacity.Ui

Item {
   id: root
   width: 15
   height: 32

   objectName: "ToolbarSeparator"

   Rectangle {
      id: separator
      width: 1
      height: 28
      anchors.centerIn: parent
      color: ui.theme.strokeColor1
      opacity: ui.theme.opacityMedium
   }
}
