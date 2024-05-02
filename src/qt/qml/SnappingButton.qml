import QtQuick
import QtQuick.Controls

import Audacity.UiComponents
import Audacity.Ui

Rectangle {
   id: root
   implicitWidth: 44
   implicitHeight: 32
   width: implicitWidth
   height: implicitHeight
   color: backgroundColor
   objectName: "SnappingButton"

   property color backgroundColor: ui.theme.backgroundColor2
   property color buttonColor: ui.theme.buttonColor
   property color strokeColor: ui.theme.strokeColor2
   property color brandColor: ui.theme.brandColor
   property bool snapped: false

   signal clicked()

   Rectangle {
      id: snappingButton
      width: 32
      height: 32
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
               color: snapped ? brandColor : buttonColor
               opacity: 0.5
            }
         },

         State {
            name: "ACTIVE"
            when: snapped

            PropertyChanges {
               target: snappingButton
               color: brandColor
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
      color: ui.theme.fontColor1
      font.family: ui.theme.iconFont.family
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
      height: 32
      radius: 0
      transparent: true
      text: String.fromCharCode(IconCode.SMALL_ARROW_DOWN)
      textFont.family: ui.theme.iconFont.family
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
