import QtQuick
import QtQuick.Controls

import Audacity
import Audacity.UiComponents

Item {
   id: root
   width: implicitWidth
   height: implicitHeight
   implicitWidth: 100
   implicitHeight: 32
   objectName: "ScrollableRuler"

   signal contentChanged(real playheadCursorPosition, int scrolledTo)

   property int contentWidth: root.width

   QtObject {
      id: prv
      property bool resetPlayheadCursorPosition: true
   }

   function start() {
      if (prv.resetPlayheadCursorPosition) {
         scrollView.contentItem.contentX = 0
         prv.resetPlayheadCursorPosition = false
      }
      timelineRuler.start()
   }

   function pause() {
      timelineRuler.pause()
   }

   function stop() {
      prv.resetPlayheadCursorPosition = true
      timelineRuler.stop()
   }

   function scrollTo(x) {
      scrollView.contentItem.contentX = x
   }

   ScrollView {
      id: scrollView
      anchors.fill: parent

      TimelineRuler {
         id: timelineRuler
         x: 1
         width: contentWidth
         playheadCursorHeight: root.height / 2

         onPlayheadCursorPositionChanged: x => {
            let currentPage = Math.floor(parseInt(x) / scrollView.width)
            let potentialNewContentWidth = (currentPage + 1) * scrollView.width

            if (potentialNewContentWidth > root.contentWidth) {
               root.contentWidth = potentialNewContentWidth
            }

            scrollView.contentItem.contentX = currentPage * scrollView.width
            root.contentChanged(x,  scrollView.contentItem.contentX)
         }
      }
   }
}
