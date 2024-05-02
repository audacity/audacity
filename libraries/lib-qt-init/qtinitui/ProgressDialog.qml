import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import QtQuick.Dialogs

import Audacity
import Audacity.UiComponents
import Audacity.Ui

Window
{
   id : dialog

   modality : Qt.ApplicationModal
   flags : Qt.Dialog

   color: ui.theme.backgroundColor2

   minimumWidth : 400
   minimumHeight : Math.max(128, contentLayout.Layout.minimumHeight)

   width : Math.max(minimumWidth, contentLayout.Layout.minimumWidth)
   maximumHeight : minimumHeight

   QtObject {
      id : internal
      property double startTime : 0
   }

   onVisibleChanged : internal.startTime = Date.now()

   property int result : 0

   property alias text : message.text
   property alias from : progress.from
   property alias to : progress.to
   property alias value : progress.value
   property alias stopVisible : stop.visible
   property bool confirmStopOrCancel : false

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
      RowLayout {
         spacing : 8

         StyledProgressBar {
            Layout.fillWidth : true
            id : progress
         }

         FlatButton {
            Layout.alignment : Qt.AlignVCenter
            
            id : stop
            text : "Stop"

            height: 24
            buttonType : FlatButton.Horizontal
            textFont.pixelSize: 12

            onClicked : function() {
               if(confirmStopOrCancel)
                  confirmStopDialog.visible = true
               else
                  dialog.result = DialogButtonBox.Abort
            }
         }
      }
      Text {
         id : remaining
         color: ui.theme.fontColor1
         font.family: ui.theme.bodyFont.family
      }

      Timer {
         interval: 1000
         repeat : true
         running : true
         onTriggered: function() {
            var dt = Date.now() - internal.startTime

            var timeFormat = function(seconds) {
               var hh = Math.floor(seconds / 3600)
               seconds -= hh * 3600
               var mm = Math.floor(seconds / 60 )
               var ss = Math.floor(seconds - mm * 60)
               if(mm < 10)
                  mm = "0" + mm
               if(ss < 10)
                  ss = "0" + ss
               return "%1:%2:%3".arg(hh).arg(mm).arg(ss)
            }

            if(dialog.value == 0)
               remaining.text = "--:--:--"
            else
               remaining.text = timeFormat(dt * ((dialog.to - dialog.from) / (dialog.value - dialog.from) - 1) / 1000);
            remaining.text += " left"
         }
      }
   }

   onClosing : function(event) {
      event.accepted = false
      if(confirmStopOrCancel)
         confirmCancelDialog.visible = true
      else
         dialog.result = DialogButtonBox.Abort
   }

   SimpleConfirmDialog {
      id : confirmCancelDialog
      text : "Are you sure you wish to cancel?"
      onAccepted : dialog.result = DialogButtonBox.Cancel
   }

   SimpleConfirmDialog {
      id : confirmStopDialog
      text : "Are you sure you wish to stop?"
      onAccepted : dialog.result = DialogButtonBox.Cancel
   }
}
