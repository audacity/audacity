import QtQuick
import QtQuick.Controls

import Audacity.Ui

ProgressBar {
   id: control
   height : 8

   background: Rectangle {
      implicitWidth : parent.width
      implicitHeight : 8

      radius: 8
      color: ui.theme.buttonColor
   }
   contentItem : Item {
      implicitWidth : parent.width
      implicitHeight : 8

      Rectangle {
         width: control.visualPosition * parent.width
         height: parent.height
         color: ui.theme.accentColor
         radius: 8
      }
   }
}
