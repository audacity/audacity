import QtQuick
import QtQuick.Layouts

import Audacity.Ui
import Audacity.UiComponents

Window {

   id : dialog

   property alias text : message.text

   modality : Qt.ApplicationModal
   flags : Qt.Dialog | Qt.CustomizeWindowHint

   color: ui.theme.backgroundColor2

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

         color: ui.theme.fontColor1
         font.family: ui.theme.bodyFont.family
      }
      StyledProgressBar {
         Layout.fillWidth : true
         indeterminate : true
      }
   }
}
