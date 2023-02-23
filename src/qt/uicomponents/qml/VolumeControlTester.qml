import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Audacity.UiComponents

Item {
   id: root
   width: contents.width
   objectName: "VolumeControlTester"

   Layout.alignment: Qt.AlignVCenter

   property alias enableRandom: sendRandom.checked
   property int channels: 1

   signal sendData(channel: int, data: double)
   signal sendRandomData(data: double)
   signal reset()

   Timer {
      id: sendingDataTimer
      interval: sendingInterval.value
      repeat: true

      onTriggered: {
         for (var i = 0; i < channels; i++) {
            var data = Math.floor(Math.random() * 1001) / 1000.0
            root.sendRandomData(data)
         }
      }
   }

   ColumnLayout {
      id: contents
      spacing: 0

      RowLayout {
         CheckBox {
            id: sendRandom
            text: qsTr("Random")
            checked: false

            onCheckedChanged: {
               sendingDataTimer.running = checked
            }
         }

         SpinBox {
            id: sendingInterval
            implicitWidth: 60
            value: 100
            from: 10
            to: 1000
            stepSize: 10
            enabled: !sendRandom.checked
            editable: true

            textFromValue: function(value) {
               return value
            }
         }

         Label {
            text: qsTr("ms")
         }

         Button {
            id: reset
            text: qsTr("Reset")

            onClicked: {
               sendRandom.checked = false
               root.reset()
            }
         }
      }

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
            enabled: !sendRandom.checked
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
            enabled: !sendRandom.checked
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

         Button {
            id: send
            text: qsTr("Send")
            enabled: !sendRandom.checked

            onClicked: {
               root.sendData(0, leftChannel.displayText)
               root.sendData(1, rightChannel.displayText)
            }
         }
      }
   }
}
