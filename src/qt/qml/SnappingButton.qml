import QtQuick
import QtQuick.Controls

import Audacity.UiComponents

Rectangle {
   id: root
   implicitWidth: 40
   implicitHeight: 28
   width: implicitWidth
   height: implicitHeight
   color: backgroundColor
   objectName: "SnappingButton"

   property color backgroundColor: appConfig.backgroundColor2
   property color buttonColor: appConfig.buttonColor
   property color strokeColor: appConfig.strokeColor
   property color accentColor: appConfig.accentColor
   property bool snapped: false

   signal clicked()

   Rectangle {
      id: snappingButton
      width: 28
      height: 28
      color: backgroundColor
      anchors.left: parent.left

      states: [
         State {
            name: "PRESSED"
            when: mouseArea.pressed

            PropertyChanges {
               target: snappingButton
               color: buttonColor
               opacity: 1.0
            }
         },

         State {
            name: "HOVER"
            when: mouseArea.containsMouse && !mouseArea.pressed

            PropertyChanges {
               target: snappingButton
               color: snapped ? accentColor : buttonColor
               opacity: 0.5
            }
         },

         State {
            name: "ACTIVE"
            when: snapped

            PropertyChanges {
               target: snappingButton
               color: accentColor
               opacity: 0.7
            }
         }
      ]

      MouseArea {
         id: mouseArea
         anchors.fill: parent
         hoverEnabled: true

         onClicked: {
            snapped = !snapped
            root.clicked()
         }
      }
   }

   Text {
      anchors.fill: snappingButton
      horizontalAlignment: Text.AlignHCenter
      verticalAlignment: Text.AlignVCenter
      text: String.fromCharCode(IconCode.MAGNET)
      color: appConfig.fontColor1
      font.family: appConfig.iconFont.family
      font.pixelSize: 14
   }

   Rectangle {
      width: 1
      height: parent.height
      color: strokeColor
      anchors.left: parent.left
   }

   Rectangle {
      width: 1
      height: parent.height
      color: strokeColor
      anchors.right: snappingIconButton.left
   }

   FlatButton {
      id: snappingIconButton
      width: 12
      height: 28
      radius: 0
      transparent: true
      text: String.fromCharCode(IconCode.SMALL_ARROW_DOWN)
      textFont.family: appConfig.iconFont.family
      textFont.pixelSize: 16
      anchors.right: parent.right
   }

   Rectangle {
      width: parent.width
      height: 1
      color: strokeColor
      anchors.bottom: parent.bottom
   }
}
