import QtQuick

import Audacity.UiThemes

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
      color: UiTheme.strokeColor
   }
}
