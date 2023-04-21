import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Audacity.UiComponents

Item {
   id: root
   width: contents.width
   objectName: "VolumeControlTester"

   property int channels: 1

   signal sendData(channel: int, data: double)
   signal sendRandomData(data: double)
   signal reset()

   function startDemo() {
      sendingDataTimer.start()
   }

   function stopDemo() {
      sendingDataTimer.stop()
      root.reset()
   }

   function pauseDemo() {
      sendingDataTimer.stop()
   }

   Timer {
      id: sendingDataTimer
      interval: 100
      repeat: true

      onTriggered: {
         for (var i = 0; i < channels; i++) {
            var data = Math.floor(Math.random() * 1001) / 1000.0
            root.sendRandomData(data)
         }
      }
   }

   RowLayout {
      id: contents
      anchors.centerIn: parent
      Layout.alignment: Qt.AlignVCenter

      ColumnLayout {
         spacing: 1

         RowLayout {
            Label {
               text: qsTr("L")
            }

            SpinBox {
               id: leftChannel
               implicitWidth: 60
               value: 50
               from: 0
               to: 100
               stepSize: 1
               enabled: !sendingDataTimer.running
               editable: true

               validator: DoubleValidator {
                  bottom: 0.0
                  top: 1.0
                  decimals: 2
               }

               textFromValue: function(value) {
                  return (value / 100).toFixed(2)
               }

               valueFromText: function(text) {
                  return Number(text) * 100
               }
            }
         }

         RowLayout {
            Label {
               text: qsTr("R")
            }

            SpinBox {
               id: rightChannel
               implicitWidth: 60
               value: 50
               from: 0
               to: 100
               stepSize: 1
               enabled: !sendingDataTimer.running
               editable: true

               validator: DoubleValidator {
                  bottom: 0.0
                  top: 1.0
                  decimals: 2
               }

               textFromValue: function(value) {
                  return (value / 100).toFixed(2)
               }

               valueFromText: function(text) {
                  return Number(text) * 100
               }
            }
         }
      }

      Button {
         id: send
         text: qsTr("Send")
         enabled: !sendingDataTimer.running

         onClicked: {
            root.sendData(0, leftChannel.displayText)
            root.sendData(1, rightChannel.displayText)
         }
      }
   }
}
