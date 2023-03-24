import QtQuick
import QtQuick.Controls

import Audacity.UiComponents

Rectangle {
   id: root
   height: 6
   width: 100
   color: appConfig.buttonColor
   objectName: "MeterPanel"

   property alias value: meterPanelHandler.value
   property alias maximumRecentPeaksCount: recentPeaks.maximumCount

   // value: [0..1]
   function calculatePeak(value) {
      return root.width * value
   }

   function updatePeaks(value) {
      var peak = calculatePeak(value)
      if (peak > clippingZone.xOffset) {
         peaks.clippedCount += 1
         if (peaks.clippedCount >= 4) {
            clippingZoneArea.opacity = 1.0
            clippingZoneArea.color = appConfig.recordColor
            currentPeakIndicator.color = appConfig.recordColor
         } else {
            currentPeakIndicator.color = appConfig.accentColor
         }
         peaks.current = clippingZone.xOffset
      } else {
         peaks.clippedCount = 0
         currentPeakIndicator.color = appConfig.accentColor
         peaks.current = peak
      }

      if (recentPeaks.values.length >= maximumRecentPeaksCount) {
         recentPeaks.values.shift()
      }
      recentPeaks.values.push(peaks.current - 1)
      peaks.recent = Math.max(...recentPeaks.values)

      var potentialNewMax = peaks.current - 1
      if (potentialNewMax >= peaks.maximum) {
         peaks.maximum = potentialNewMax
      }
   }

   function reset() {
      recentPeaks.values.length = 0
      meterPanelHandler.Reset()
   }

   QtObject {
      id: clippingZone
      readonly property int width: 4
      readonly property int xOffset: root.width - width
   }

   QtObject {
      id: defaultPeaks
      readonly property int current: 0
      readonly property int recent: -1
      readonly property int maximum: -1
      readonly property int clippedCount: 0
   }

   QtObject {
      id: peaks
      property int current: defaultPeaks.current
      property int recent: defaultPeaks.recent
      property int maximum: defaultPeaks.maximum
      property int clippedCount: defaultPeaks.clippedCount
   }

   QtObject {
      id: recentPeaks
      property int maximumCount: 10
      property var values: []
   }

   MeterPanelHandler {
      id: meterPanelHandler

      onValueChanged: function(value) {
         updatePeaks(value)
      }

      onResetIndicators: function() {
         peaks.current = defaultPeaks.current
         peaks.recent = defaultPeaks.recent
         peaks.maximum = defaultPeaks.maximum
         peaks.clippedCount = defaultPeaks.clippedCount

         clippingZoneArea.opacity = 0.1
         clippingZoneArea.color = appConfig.backgroundColor3
      }
   }

   Rectangle {
      id: currentPeakIndicator
      height: parent.height
      width: peaks.current
      color: appConfig.accentColor
   }

   Rectangle {
      id: recentPeakIndicator
      x: peaks.recent
      height: parent.height
      width: 1
      color: appConfig.accentColor
      visible: peaks.recent !== -1
   }

   Rectangle {
      id: maximumPeakIndicator
      x: peaks.maximum
      height: parent.height
      width: 1
      color: appConfig.fontColor1
      visible: peaks.maximum !== -1
   }

   Rectangle {
      id: clippingZoneArea
      x: parent.width - clippingZone.width
      height: parent.height
      width: clippingZone.width
      opacity: 0.1
      color: appConfig.backgroundColor3
   }

   Component.onCompleted: {
      updatePeaks(meterPanelHandler.value);
   }
}
