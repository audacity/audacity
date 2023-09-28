import QtQuick
import QtQuick.Controls

import Audacity.UiThemes

Slider {
   id: root
   value: 0.5

   Rectangle {
      x: root.height / 2
      y: root.availableHeight / 2 - height / 2
      width: root.visualPosition * fill.width
      height: fill.height
      color: UiTheme.brandColor
      radius: 2
   }

   background: Rectangle {
      id: fill
      x: root.height / 2
      y: root.availableHeight / 2 - height / 2
      width: root.availableWidth - handleId.width
      height: 4
      radius: 2
      color: UiTheme.strokeColor1
      opacity: UiTheme.opacityLight
   }

   handle: Rectangle {
      id: handleId
      x: root.visualPosition * (root.availableWidth - width)
      y: root.availableHeight / 2 - height / 2
      z: 1
      implicitWidth: 16
      implicitHeight: 16
      radius: 8
      color: UiTheme.backgroundColor2
      border.color: UiTheme.strokeColor1
      antialiasing: true
   }
}
