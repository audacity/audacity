import QtQuick
import QtQuick.Controls

import Audacity.UiComponents
import Audacity.UiThemes

Item {
   id: root
   width: 158
   height: 28
   objectName: "VolumeControl"

   readonly property int meterPanelWidth: 120

   function setLeftChannelValue(value) {
      volumeControlHandler.leftValue = value
   }

   function setRightChannelValue(value) {
      volumeControlHandler.rightValue = value
   }

   function reset() {
      topMeterPanel.reset()
      bottomMeterPanel.reset()
   }

   function setVolume(value) {
      handle.x = value * meterPanelWidth
   }

   VolumeControlHandler {
      id: volumeControlHandler

      onResetIndicators: function() {
         reset()
      }

      onVolumeChanged: function(volume) {
         setVolume(volume)
      }

      onLeftValueChanged: function(value) {
         topMeterPanel.value = value
      }

      onRightValueChanged: function(value) {
         bottomMeterPanel.value = value
      }
   }

   Text {
      id: speaker
      width: 16
      height: 16
      anchors.verticalCenter: parent.verticalCenter
      color: UiTheme.fontColor1

      text: {
         if (handle.x === 0) {
            return String.fromCharCode(IconCode.SPEAKER_OFF)
         } else {
            return String.fromCharCode(IconCode.SPEAKER_ON)
         }
      }

      font {
         family: appConfig.iconFont.family
         pixelSize: 16
      }
   }

   Item {
      id: sliderBoundingBox
      x: 20
      width: 138
      height: 18
      anchors.verticalCenter: parent.verticalCenter

      MeterPanel {
         id: topMeterPanel
         x: 9
         y: 2
         width: meterPanelWidth
         maximumRecentPeaksCount: 50
      }

      MeterPanel {
         id: bottomMeterPanel
         x: 9
         y: 10
         width: meterPanelWidth
         maximumRecentPeaksCount: 50
      }

      Rectangle {
         id: handle
         width: 18
         height: 18
         opacity: 0.4
         color: "white"
         radius: width / 2
         anchors.verticalCenter: parent.verticalCenter

         border {
             color: UiTheme.fontColor1
             width: 1
         }

         onXChanged: volumeControlHandler.ChangeVolume(handle.x / meterPanelWidth)
      }

      DragHandler {
         id: dragHandler
         target: handle
         xAxis.minimum: 0
         xAxis.maximum: sliderBoundingBox.width - handle.width
      }
   }

   Component.onCompleted: {
      setVolume(volumeControlHandler.volume)
      topMeterPanel.value = volumeControlHandler.leftValue
      bottomMeterPanel.value = volumeControlHandler.rightValue
   }
}
