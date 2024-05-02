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

   property var buttons : []
   property int result : 0
   property alias text : message.text
   property alias groupTitle : group.title

   Component {
      id : buttonFactory

      RadioButton {
         id : control
         font {
            family : ui.theme.bodyFont.family
            pixelSize : 12
         }
         property int tag
         contentItem: Text {
            text: control.text
            font: control.font
            color: ui.theme.fontColor1
            leftPadding: control.indicator.width + control.spacing
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

      GroupBox
      {
         id : group

         Layout.fillWidth : true
         Layout.leftMargin : 16
         Layout.rightMargin : 16
         Layout.bottomMargin : 16

         background: Rectangle {
            y: group.topPadding - group.bottomPadding
            width: parent.width
            height: parent.height - group.topPadding + group.bottomPadding
            color: "transparent"
            border.color: ui.theme.strokeColor
            radius: 3
         }

         label.visible : false

         ColumnLayout
         {
            anchors.fill : parent
            anchors.margins : 8
            id : buttonsLayout
         }

         Component.onCompleted : {
            for(var i = 0; i < dialog.buttons.length; ++i)
            {
               buttonFactory.createObject(buttonsLayout, {
                  tag : i,
                  text : dialog.buttons[i],
                  checked : i == dialog.result
               })
            }
         }
      }

      FlatButton {
         Layout.leftMargin : 16
         Layout.rightMargin : 16
         Layout.bottomMargin : 16

         buttonType : FlatButton.Horizontal
         textFont.pixelSize: 12

         text : "OK"

         onClicked : dialog.close()
      }
   }

   onClosing : function() {
      for(var i = 0; i < buttonsLayout.children.length; ++i)
      {
         if(buttonsLayout.children[i].checked)
         {
            dialog.result = i;
            break;
         }
      }
   }

}
