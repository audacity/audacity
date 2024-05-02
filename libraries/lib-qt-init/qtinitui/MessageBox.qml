import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Audacity.UiComponents
import Audacity.Ui

Window {
   id : dialog

   modality : Qt.ApplicationModal
   flags : Qt.Dialog

   color: ui.theme.backgroundColor2

   minimumWidth : Math.max(250, rootLayout.Layout.minimumWidth)
   minimumHeight : Math.max(128, rootLayout.Layout.minimumHeight)
   maximumWidth : minimumWidth
   maximumHeight : minimumHeight

   property int buttons : 0 
   property int result : 0
   property alias text : message.text

   Component {
      id : buttonFactory

      FlatButton {
         Layout.fillWidth : true
         Layout.alignment : Qt.AlignRight

         height: 24
         buttonType : FlatButton.Horizontal
         textFont.pixelSize: 12

         property int tag

         onClicked : {
            dialog.result = tag
            dialog.close()
         }
      }
   }
   ColumnLayout
   {
      id : rootLayout

      anchors.fill : parent

      Text {
         id : message
         padding : 16

         color: ui.theme.fontColor1
         font.family: ui.theme.bodyFont.family
      }

      RowLayout
      {
         id : buttonsLayout
         spacing : 8
         Layout.leftMargin : 16
         Layout.rightMargin : 16
         Layout.bottomMargin : 16

         Component.onCompleted : {
            if(dialog.buttons & DialogButtonBox.Ok)
               buttonFactory.createObject(buttonsLayout, { tag : DialogButtonBox.Ok, text : "OK" })
            if(dialog.buttons & DialogButtonBox.Yes)
               buttonFactory.createObject(buttonsLayout, { tag : DialogButtonBox.Yes, text : "Yes" })
            if(dialog.buttons & DialogButtonBox.No)
               buttonFactory.createObject(buttonsLayout, { tag : DialogButtonBox.No, text : "No" })
            if(dialog.buttons & DialogButtonBox.Cancel)
               buttonFacotry.createObject(buttonsLayout, { tag : DialogButtonBox.Cancel, text : "Cancel" })
         }
      }
   }

}
