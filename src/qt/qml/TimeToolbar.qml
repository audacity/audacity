import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Audacity
import Audacity.UiComponents

Item {
   id: root
   height: parent.height
   implicitHeight: height
   implicitWidth: contents.width

   objectName: "TimeToolbar"

   property bool testerVisible: false
   property bool gripVisible: false
   property bool separatorVisible: false

   signal updateStatusBar(status: string)

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

         TimeControlTester {
            id: timeControlTester
            visible: testerVisible

            onStart: timeControl.start()
            onStop: timeControl.stop()
            onReset: timeControl.reset()
         }
      }

      ToolbarSeparator {
         visible: separatorVisible
      }
   }
}
