import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Audacity.UiComponents

Item {
   id: root
   width: contents.width
   height: 28
   objectName: "TimeControlTester"

   property bool started: false

   signal start()
   signal stop()
   signal reset()

   RowLayout {
      id: contents
      spacing: 1

      Button {
         id: start
         text: qsTr("Start")
         enabled: !started

         onClicked: {
            started = true
            root.start()
         }
      }

      Button {
         id: stop
         text: qsTr("Stop")
         enabled: started

         onClicked: {
            started = false
            root.stop()
         }
      }

      Button {
         id: reset
         text: qsTr("Reset")

         onClicked: {
            started = false
            root.reset()
         }
      }
   }
}
