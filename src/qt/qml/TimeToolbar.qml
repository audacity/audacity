import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Audacity
import Audacity.UiComponents

Item {
   id: root
   height: implicitHeight
   implicitHeight:48
   implicitWidth: contents.width

   objectName: "TimeToolbar"

   property bool gripVisible: false
   property bool separatorVisible: false

   signal updateStatusBar(status: string)

   function start() {
      timeControl.start()
   }

   function stop() {
      timeControl.reset()
   }

   function pause() {
      timeControl.stop()
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
            color: appConfig.backgroundColor1
         }

         TimeControl {
            id: timeControl
         }
      }

      ToolbarSeparator {
         visible: separatorVisible
      }
   }
}
