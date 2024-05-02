import QtQuick
import QtQuick.Controls

import Audacity.Ui

Slider {
   id: root
   value: 0.5

   Rectangle {
      x: root.height / 2
      y: root.availableHeight / 2 - height / 2
      width: root.visualPosition * fill.width
      height: fill.height
      color: ui.theme.brandColor
      radius: 2
   }

   background: Rectangle {
      id: fill
      x: root.height / 2
      y: root.availableHeight / 2 - height / 2
      width: root.availableWidth - handleId.width
      height: 4
      radius: 2
      color: ui.theme.strokeColor1
      opacity: ui.theme.opacityLight
   }

   handle: Rectangle {
      id: handleId
      x: root.visualPosition * (root.availableWidth - width)
      y: root.availableHeight / 2 - height / 2
      z: 1
      implicitWidth: 16
      implicitHeight: 16
      radius: 8
      color: ui.theme.backgroundColor2
      border.color: ui.theme.strokeColor1
      antialiasing: true
   }
}
