import QtQuick
import QtQuick.Controls

import Audacity
import Audacity.UiComponents
import Audacity.UiThemes

Rectangle {
   id: root
   implicitHeight: 32
   height: implicitHeight
   color: UiTheme.backgroundColor2
   objectName: "TimelineRuler"

   property alias playheadCursorHeight: playheadCursor.height
   property bool snapped: false
   property color separatorColor: UiTheme.fontColor1
   property color textColor: UiTheme.fontColor1
   property font textFont: UiTheme.bodyFont

   function start() {
      if (playheadMovementAnimation.paused) {
         playheadMovementAnimation.resume()
      } else {
         playheadCursor.x = 5
         playheadCursor.visible = true
         playheadMovementAnimation.start()
      }
   }

   function pause() {
      playheadMovementAnimation.pause()
   }

   function stop() {
      playheadMovementAnimation.stop()
      playheadCursor.visible = false
   }

   function updateTheme() {
      ruler.UpdateTheme()
   }

   Rectangle {
      id: playheadRecessSeparator
      x: 12
      width: 1
      height: parent.height
      color: separatorColor
      visible: false
   }

   Rectangle {
      height: 1
      color: UiTheme.strokeColor2
      y: parent.height / 2
      anchors.left: playheadRecessSeparator.right
      anchors.right: snappingButton.left
   }

   Rectangle {
      id: separator
      y: parent.height - 1
      width: parent.width
      height: 1
      color: UiTheme.strokeColor2
   }

   AdornedRulerPanel {
      id: ruler
      offset: playheadRecessSeparator.x
      anchors.fill: parent
   }

   PlayheadCursor {
      id: playheadCursor
      x: 5
      y: root.height / 2
      height: root.height
      visible: false

      PropertyAnimation on x {
         id: playheadMovementAnimation
         running: false
         loops: Animation.Infinite
         from: playheadCursor.x
         to: root.width
         duration: (root.width / 80) * 1000
      }
   }

   SnappingButton {
      id: snappingButton
      anchors.right: parent.right
   }
}
