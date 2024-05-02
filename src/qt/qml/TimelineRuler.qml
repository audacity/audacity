import QtQuick
import QtQuick.Controls

import Audacity
import Audacity.UiComponents
import Audacity.Ui

Rectangle {
   id: root
   implicitHeight: 32
   height: implicitHeight
   color: ui.theme.backgroundColor2
   objectName: "TimelineRuler"
   clip: false

   signal playheadCursorPositionChanged(real x)

   property alias playheadCursorHeight: playheadCursor.height
   property color separatorColor: ui.theme.fontColor1
   property color textColor: ui.theme.fontColor1
   property font textFont: ui.theme.bodyFont

   QtObject {
      id: prv
      property int previousPlayheadCursorX
   }

   function start() {
      if (playheadMovementAnimation.paused) {
         playheadMovementAnimation.resume()
      } else {
         playheadCursor.x = 5
         playheadCursor.visible = true
         prv.previousPlayheadCursorX = parseInt(playheadCursor.x)
         playheadMovementAnimation.start()
      }
   }

   function pause() {
      playheadMovementAnimation.pause()
   }

   function stop() {
      playheadMovementAnimation.stop()
      playheadCursor.visible = false
      prv.previousPlayheadCursorX = 0
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
      color: ui.theme.strokeColor2
      y: parent.height / 2
      anchors.left: playheadRecessSeparator.right
      anchors.right: parent.right
   }

   Rectangle {
      id: separator
      y: parent.height - 1
      width: parent.width
      height: 1
      color: ui.theme.strokeColor2
   }

   AdornedRulerPanel {
      id: ruler
      offset: playheadRecessSeparator.x
      anchors.fill: parent
      textFont: root.textFont
      textColor: root.textColor
      tickColor: root.separatorColor
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
         to: 100000000
         duration: 13.6 * playheadMovementAnimation.to
      }

      onXChanged: {
         root.playheadCursorPositionChanged(playheadCursor.x)
      }
   }
}
