import QtQuick
import QtQuick.Layouts

import Audacity.UiThemes
import Audacity.UiComponents

Window {

   id : dialog

   property alias text : message.text

   modality : Qt.ApplicationModal
   flags : Qt.Dialog | Qt.CustomizeWindowHint

   color: UiTheme.backgroundColor2

   minimumWidth : 400
   minimumHeight : 128
   maximumHeight : 128

   ColumnLayout
   {
      id : contentLayout

      anchors.margins : 16
      anchors.fill : parent

      spacing : 0

      Text {
         id : message

         color: UiTheme.fontColor1
         font.family: UiTheme.bodyFont.family
      }
      StyledProgressBar {
         Layout.fillWidth : true
         indeterminate : true
      }
   }
}
