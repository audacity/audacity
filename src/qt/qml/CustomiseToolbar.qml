import QtQuick
import QtQuick.Controls
import Qt.labs.qmlmodels

import Audacity
import Audacity.UiComponents
import Audacity.Ui

ApplicationWindow {
   id: root
   objectName: "Customise Toolbar"
   width: 272
   height: 400
   color: ui.theme.backgroundColor1

   header: Item {
      id: label
      x: 16
      width: parent.width
      height: 36

      Text {
         id: title
         anchors.fill: parent
         text: qsTr("Customise toolbar")
         color: ui.theme.fontColor1
         verticalAlignment: Text.AlignVCenter

         font {
            family: ui.theme.bodyFont.family
            pixelSize: 12
         }
      }
   }

   DelegateChooser {
      id: delegateChooser
      role: "type"

      DelegateChoice {
         roleValue: "separator"

         Component {
            id: separator

            Rectangle {
               width: root.width
               height: 8
               color: ui.theme.backgroundColor1

               Rectangle {
                  x: 8
                  width: parent.width - 16
                  height: 1
                  anchors.verticalCenter: parent.verticalCenter
                  color: ui.theme.strokeColor1
                  opacity: ui.theme.opacityLight
               }
            }
         }
      }

      DelegateChoice {
         roleValue: "configuration"

         Component {
            id: configurationItem

            Item {
               id: container
               width: root.width
               height: 32

               Rectangle {
                  id: background
                  anchors.fill: parent

                  states: [
                     State {
                        name: "DEFAULT"
                        when: !mouseArea.containsMouse
                        PropertyChanges { target: background; color: ui.theme.backgroundColor1; opacity: 1.0 }
                     },
                     State {
                        name: "HOVER"
                        when: mouseArea.containsMouse && !mouseArea.pressed
                        PropertyChanges { target: background; color: ui.theme.backgroundColor2; opacity: 0.7 }
                     },
                     State {
                        name: "PRESSED"
                        when: mouseArea.containsMouse && mouseArea.pressed
                        PropertyChanges { target: background; color: ui.theme.backgroundColor2; opacity: 1.0 }
                     }
                  ]

                  MouseArea {
                     id: mouseArea
                     anchors.fill: parent
                     acceptedButtons: Qt.LeftButton
                     hoverEnabled: true
                     onClicked: {
                        model.visible = !model.visible
                     }
                  }
               }

               Text {
                  id: visibility
                  x: 8
                  width: 32
                  height: 32
                  color: ui.theme.fontColor1
                  verticalAlignment: Text.AlignVCenter
                  horizontalAlignment: Text.AlignHCenter

                  font {
                     family: ui.theme.iconFont.family
                     pixelSize: 16
                  }

                  text: {
                     if (model.visible) {
                        return String.fromCharCode(IconCode.VISIBILITY_ON)
                     } else {
                        return String.fromCharCode(IconCode.VISIBILITY_OFF)
                     }
                  }
               }

               Text {
                  id: icon
                  x: 48
                  width: 32
                  height: 32
                  color: model.iconColor
                  verticalAlignment: Text.AlignVCenter
                  horizontalAlignment: Text.AlignHCenter

                  font {
                     family: ui.theme.iconFont.family
                     pixelSize: 14
                  }

                  text: {
                     return String.fromCharCode(model.icon)
                  }
               }

               Text {
                  id: description
                  text: model.description
                  color: ui.theme.fontColor1
                  anchors.left: icon.right
                  anchors.right: parent.right
                  anchors.top: parent.top
                  anchors.bottom: parent.bottom
                  verticalAlignment: Text.AlignVCenter

                  font {
                     family: ui.theme.bodyFont.family
                     pixelSize: 12
                  }
               }
            }
         }
      }
   }

   ListView {
      id: listView
      width: parent.width
      height: root.height - label.height
      model: ToolbarManager
      delegate: delegateChooser
      boundsBehavior: Flickable.StopAtBounds
      clip: true

      ScrollBar.vertical: ScrollBar {
         anchors.right: listView.right
      }
   }
}
