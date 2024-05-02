import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Audacity
import Audacity.UiComponents
import Audacity.Ui
import Audacity.NumericFormats

Item {
   id: root
   height: implicitHeight
   implicitHeight:48
   implicitWidth: contents.width
   visible: toolbarHandler.toolbarVisible

   objectName: "TimeToolbar"

   property bool gripVisible: false
   property bool separatorVisible: false

   signal updateStatusBar(status: string)

   property double lastUpdateTime : 0

   function start() {
      lastUpdateTime = Date.now()
      timer.start()
   }

   function stop() {
      projectTime.value = 0
      timer.stop()
   }

   function pause() {
      timer.stop()
   }

   Timer {
      id: timer
      interval: 1
      repeat: true

      onTriggered: {
         var now = Date.now()
         projectTime.value += (now - lastUpdateTime) / 1000
         lastUpdateTime = now
      }
   }

   function registerToolbarConfiguration() {
      toolbarHandler.RegisterToolbarConfiguration()
   }

   TimeToolbarHandler {
      id: toolbarHandler
   }

   RowLayout {
      id: contents
      anchors.verticalCenter: parent.verticalCenter
      spacing: 8

      ToolbarGrip {
         id: grip
         visible: root.gripVisible
      }

      RowLayout {
         spacing: 2

         Rectangle {
            height: root.height
            width: root.gripVisible ? 2 : 6
            color: ui.theme.backgroundColor1
         }

         NumericTextControl {
            id : projectTime
            Layout.fillWidth : true
            implicitHeight : 28
            type : "time"
         }
      }

      ToolbarSeparator {
         visible: separatorVisible
      }
   }

   Component.onCompleted: {
      toolbarHandler.StoreToolbarManager(ToolbarManager)
   }
}
