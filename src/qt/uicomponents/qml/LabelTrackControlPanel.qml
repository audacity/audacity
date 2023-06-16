import QtQuick
import QtQuick.Layouts

import Audacity
import Audacity.UiComponents

Item {
   id: root
   objectName: "LabelTrackControlPanel"
   height: implicitHeight
   width: implicitWidth
   implicitHeight: 86
   implicitWidth: 280

   property string name: ""

   signal optionsClicked()
   signal addClicked()

   Rectangle {
      id: background
      anchors.fill: parent
      color: appConfig.backgroundColor2
      opacity: 1.0

      states: [
         State {
            name: "HOVERED"
            when: mouseArea.containsMouse

            PropertyChanges {
               target: background
               color: appConfig.backgroundColor1
               opacity: appConfig.buttonOpacityHover
            }
         }
      ]
   }

   MouseArea {
      id: mouseArea
      anchors.fill: parent
      hoverEnabled: true

      Item {
         Text {
            id: icon
            x: 12
            y: 14
            width: 16
            height: 16
            color: appConfig.fontColor1
            text: String.fromCharCode(IconCode.FLAG)

            font {
               family: appConfig.iconFont.family
               pixelSize: 16
            }
         }

         Text {
            x: 36
            y: 14
            height: 16
            horizontalAlignment: Text.AlignLeft
            color: appConfig.fontColor1
            text: name

            font {
               family: appConfig.bodyFont.family
               pixelSize: 12
            }
         }

         FlatButton {
            id: options
            x: 224
            y: 12
            width: 20
            height: 20
            transparent: true
            icon: IconCode.MENU_THREE_DOTS
            onClicked: root.optionsClicked()
         }

         FlatButton {
            id: add
            x: 12
            y: 44
            width: 232
            height: 28
            text: qsTr("Add marker")
            textFont.pixelSize: 12
            onClicked: root.addClicked()
         }

         Rectangle {
            id: verticalSeparator
            x: 255
            width: 1
            height: root.height
            color: appConfig.strokeColor
         }
      }
   }

   Rectangle {
      id: horizontalSeparator
      y: root.height - horizontalSeparator.height
      height: 2
      width: root.width
      color: appConfig.strokeColor
   }
}
