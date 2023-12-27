import QtQuick
import QtQuick.Controls

import Audacity
import Audacity.UiComponents
import Audacity.UiThemes

Button {
   id: root
   flat: true
   width: implicitWidth
   height: implicitHeight
   implicitWidth: 240
   implicitHeight: 28
   font.family: UiTheme.bodyFont.family
   font.pixelSize: 16

   function updateSelected(index) {
      root.text = listModel.get(index).description
      listModel.selectedIndex = index

      for (var i = 0; i < listModel.count; i++) {
         listModel.setProperty(i, "selected", i == index ? true : false)
      }
   }

   ListBoxHandler {
      id: handler
   }

   ListModel {
      id: listModel
      property int selectedIndex: -1
   }

   contentItem: Text {
      anchors.fill: parent
      text: root.text
      font: root.font
      color: UiTheme.fontColor1
      horizontalAlignment: Text.AlignHCenter
      verticalAlignment: Text.AlignVCenter
   }

   background: Rectangle {
      id: background
      radius: 3
      anchors.fill: parent
      color: (background.hovering || background.buttonPressed) ? UiTheme.backgroundColor2 : UiTheme.backgroundColor1
      opacity: background.buttonPressed ? UiTheme.opacityOpaque : (background.hovering ? UiTheme.opacityMedium : UiTheme.opacityStrong)

      property bool hovering: false
      property bool buttonPressed: false

      border {
         width: 1
         color: UiTheme.strokeColor1
      }

      MouseArea {
         hoverEnabled: true
         anchors.fill: parent
         onEntered: background.hovering = true
         onExited: background.hovering = false
         onPressed: background.buttonPressed = true
         onReleased: background.buttonPressed = false
      }
   }

   onPressed: {
      let availableGeometry = handler.GetAvailableGeometry()
      let screenCoordinates = mapToGlobal(0, 0)

      popup.x = screenCoordinates.x
      popup.y = screenCoordinates.y - (listModel.selectedIndex * root.height)
      popup.height = listModel.count * root.height

      // Adjust popup's y coordinate if any of the list entries will be offscreen
      let popupTop = popup.y
      let popupBottom = popup.y + popup.height

      if (popupTop < availableGeometry.y) {
         popup.y = availableGeometry.y
      } else if (popupBottom > availableGeometry.height) {
         popup.y = availableGeometry.height - popup.height
      }

      // Adjust popup's x coordinate if any of the list entries will be offscreen
      let popupLeft = popup.x
      let popupRight = popup.x + popup.width

      if (popupLeft < availableGeometry.x) {
         popup.x = availableGeometry.x
      } else if (popupRight > availableGeometry.width) {
         popup.x = availableGeometry.width - popup.width
      }

      popup.visible = true
   }

   Window {
      id: popup
      width: root.width
      visible: false
      color: "pink"
      flags: Qt.FramelessWindowHint
      modality: Qt.WindowModal

      ListView {
         id: listView
         anchors.fill: parent
         clip: true
         model: listModel
         focus: true
         boundsBehavior: Flickable.StopAtBounds

         Keys.onEscapePressed: {
            popup.visible = false
         }

         delegate: Rectangle {
            id: listEntry
            width: root.width
            height: root.height
            color: listEntry.hovering ? UiTheme.brandColor : UiTheme.backgroundColor2

            property bool hovering: false
            property bool buttonPressed: false

            Text {
               id: tick
               width: 28
               anchors.top: parent.top
               anchors.bottom: parent.bottom
               color: listEntry.hovering ? UiTheme.fontColor2 : UiTheme.fontColor1
               text: listModel.get(index).selected ? String.fromCharCode(IconCode.TICK) : ""
               font.family: UiTheme.iconFont.family
               font.pixelSize: 16
               horizontalAlignment: Text.AlignHCenter
               verticalAlignment: Text.AlignVCenter
            }

            Text {
               anchors.left: tick.right
               anchors.right: parent.right
               anchors.top: parent.top
               anchors.bottom: parent.bottom
               color: listEntry.hovering ? UiTheme.fontColor2 : UiTheme.fontColor1
               text: listModel.get(index).description
               font.family: UiTheme.bodyFont.family
               font.pixelSize: 16
               verticalAlignment: Text.AlignVCenter
            }

            MouseArea {
               hoverEnabled: true
               anchors.fill: parent

               onEntered: listEntry.hovering = true
               onExited: listEntry.hovering = false
               onPressed: listEntry.buttonPressed = true
               onReleased: listEntry.buttonPressed = false

               onClicked: {
                  updateSelected(index)
                  popup.visible = false
               }
            }
         }
      }
   }

   Component.onCompleted: {
      listModel.append({ "description" : "Item 1", "selected" : false })
      listModel.append({ "description" : "Item 2", "selected" : false })
      listModel.append({ "description" : "Item 3", "selected" : false })
      listModel.append({ "description" : "Item 4", "selected" : false })
      listModel.append({ "description" : "Item 5", "selected" : false })
      listModel.append({ "description" : "Item 6", "selected" : false })
      listModel.append({ "description" : "Item 7", "selected" : false })
      listModel.append({ "description" : "Item 8", "selected" : false })
      listModel.append({ "description" : "Item 9", "selected" : false })
      listModel.append({ "description" : "Item 10", "selected" : false })

      if (listModel.count > 0) {
         updateSelected(0)
      }
   }
}
