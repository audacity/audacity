import QtQuick

import Audacity.UiComponents
import Audacity.Ui

Item {
   id: root
   width: 12
   height: 48
   objectName: "ToolbarGrip"

   signal clicked()

   Rectangle {
      id: background
      anchors.fill: parent
      color: ui.theme.backgroundColor1
      opacity: 1.0

      states: [
         State {
            name: "PRESSED"
            when: mouseArea.pressed

            PropertyChanges {
               target: background
               color: ui.theme.buttonColor
               opacity: 0.4
            }
         },

         State {
            name: "HOVERED"
            when: mouseArea.containsMouse && !mouseArea.pressed

            PropertyChanges {
               target: background
               color: ui.theme.buttonColor
               opacity: 0.2
            }
         }
      ]
   }

   StyledIconLabel {
      id: gripIcon
      width: 8
      height: 16
      iconCode: IconCode.TOOLBAR_GRIP
      color: "#838393"
      font.family: ui.theme.iconFont.family
      font.pixelSize: 8
      anchors.centerIn: background
   }

   Rectangle {
      x: root.width
      id: gripEdge
      width: 1
      height: parent.height
      color: ui.theme.strokeColor1
      opacity: ui.theme.opacityMedium
   }

   MouseArea {
      id: mouseArea
      anchors.fill: parent
      hoverEnabled: true

      onClicked: function() {
         root.clicked()
      }
   }
}
