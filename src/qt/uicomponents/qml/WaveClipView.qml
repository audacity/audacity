import QtQuick
import QtQuick.Controls

import Audacity.UiComponents
import Audacity.Ui

Rectangle {
   id: root
   implicitWidth: 400
   implicitHeight: 124
   width: implicitWidth
   height: implicitHeight
   color: ui.theme.backgroundColor1
   radius: 4
   objectName: "WaveClipView"

   property alias name: label.text

   signal optionsClicked()

   states: [
      State {
         name: "LEFT_RESIZE"
         when: leftResizeMouseArea.containsMouse

         PropertyChanges { target: leftResizeMouseArea; cursorShape: Qt.SizeHorCursor }
      },
      State {
         name: "RIGHT_RESIZE"
         when: rightResizeMouseArea.containsMouse

         PropertyChanges { target: rightResizeMouseArea; cursorShape: Qt.SizeHorCursor }
      },
      State {
         name: "HOVER"
         when: mouseArea.containsMouse && !mouseArea.containsPress

         PropertyChanges { target: mouseArea; cursorShape: Qt.OpenHandCursor }
      },
      State {
         name: "DRAG"
         when: mouseArea.contains && mouseArea.containsPress

         PropertyChanges { target: mouseArea; cursorShape: Qt.ClosedHandCursor }
      }
   ]

   Text {
      id: label
      x: 4
      y: 3
      height: 12
      color: ui.theme.fontColor1
      fontSizeMode: Text.Fit
      font.family: ui.theme.bodyFont.family
      font.pixelSize: 10
   }

   MouseArea {
      id: mouseArea
      acceptedButtons: Qt.LeftButton
      anchors.top: parent.top
      anchors.left: leftResizeMouseArea.right
      anchors.right: rightResizeMouseArea.left
      anchors.bottom: clip.top
      drag.target: parent
      drag.axis: Drag.XAxis
      hoverEnabled: true
   }

   FlatButton {
      id: options
      x: label.x + label.width + 6
      y: 2
      width: 16
      height: 16
      transparent: true
      icon: IconCode.MENU_THREE_DOTS
      onClicked: root.optionsClicked()
   }

   WaveClipBody {
      id: clip
      x: 1
      y: 19
      width: root.width - 2
      height: root.height - 20
      radius: root.radius - 1
   }

   Item {
      id: leftResizeRect
      width: leftResizeMouseArea.width / 2
      height: parent.height
      anchors.left: root.left
   }

   Item {
      id: rightResizeRect
      width: rightResizeMouseArea.width / 2
      height: parent.height
      anchors.right: root.right
   }

   QtObject {
      id: prv
      property int previousX
   }

   MouseArea {
      id: leftResizeMouseArea
      x: -(width / 2)
      width: 8
      height: root.height  / 3
      hoverEnabled: true
      drag.axis: Drag.XAxis

      onPressed: {
         prv.previousX = mouseX
         drag.target = leftResizeRect
      }

      onReleased: {
         drag.target = null
         prv.previousX = 0
      }

      onMouseXChanged: {
         if (drag.target === leftResizeRect) {
            var dx = mouseX - prv.previousX
            root.x += dx
            root.width -= dx
         }
      }
   }

   MouseArea {
      id: rightResizeMouseArea
      x: root.width - (width / 2)
      width: 8
      height: root.height / 3
      hoverEnabled: true
      drag.axis: Drag.XAxis

      onPressed: {
         prv.previousX = mouseX
         drag.target = rightResizeRect
      }

      onReleased: {
         drag.target = null
         prv.previousX = 0
      }

      onMouseXChanged: {
         if (drag.target === rightResizeRect) {
            var dx = mouseX - prv.previousX
            root.width += dx
         }
      }
   }
}
