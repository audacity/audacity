import QtQuick
import QtQuick.Controls

import Audacity.UiThemes

ProgressBar {
   id: control
   height : 8

   background: Rectangle {
      implicitWidth : parent.width
      implicitHeight : 8

      radius: 8
      color: UiTheme.buttonColor
   }
   contentItem : Item {
      implicitWidth : parent.width
      implicitHeight : 8

      Rectangle {
         width: control.visualPosition * parent.width
         height: parent.height
         color: UiTheme.accentColor
         radius: 8
      }
   }
}
