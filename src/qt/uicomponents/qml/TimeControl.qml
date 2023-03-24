import QtQuick
import QtQuick.Controls

import Audacity
import Audacity.UiComponents

Rectangle {
   id: root
   height: 28
   width: formatType === TimeControl.FormatType.HoursMinutesSeconds ? 134 : 111
   radius: 3
   color: appConfig.timecodeColor
   objectName: "TimeControl"
   onFormatTypeChanged: reset()

   property alias formatType: display.formatType

   enum FormatType {
      Seconds,
      HoursMinutesSeconds
   }

   QtObject {
      id: display

      onValueChanged: update()
      onFormatTypeChanged: update()

      property var formatType: TimeControl.FormatType.HoursMinutesSeconds
      property var groupSeparator: ' '
      property int value: 0

      function displaySeconds() {
         var text = String(display.value).padStart(6, '0')
         seconds.text = text.slice(0, 3) + display.groupSeparator + text.slice(3)
      }

      function displayHoursMinutesSeconds() {
         var hrs = Math.floor(display.value / 3600)
         var mins = Math.floor((display.value - hrs * 3600) / 60)
         var secs = display.value - mins * 60 - hrs * 3600

         hours.text = String(hrs).padStart(2, '0')
         minutes.text = String(mins).padStart(2, '0')
         seconds.text = String(secs).padStart(2, '0')
      }

      function update() {
         if (formatType === TimeControl.FormatType.Seconds) {
            displaySeconds()
         } else if (formatType === TimeControl.FormatType.HoursMinutesSeconds) {
            displayHoursMinutesSeconds()
         }
      }
   }

   function setSeconds(seconds) {
      display.value = seconds
   }

   function setHoursMinutesSeconds(hours, minutes, seconds) {
      display.value = hours * 3600 + minutes * 60 + seconds
   }

   function start() {
      timer.start()
   }

   function stop() {
      timer.stop()
   }

   function reset() {
      timer.stop()
      display.value = 0
   }

   Timer {
      id: timer
      interval: 1000
      repeat: true

      onTriggered: {
         display.value += 1
      }
   }

   Text {
      id: hours
      x: 8
      width: 23
      height: 16
      anchors.verticalCenter: root.verticalCenter
      horizontalAlignment: Text.AlignRight
      color: appConfig.fontColor2
      visible: formatType === TimeControl.FormatType.HoursMinutesSeconds

      font {
         family: appConfig.bodyFont.family
         pixelSize: 16
      }
   }

   Text {
      id: hoursSuffix
      text: "h"
      x: hours.x + hours.width
      width: 10
      height: 16
      anchors.verticalCenter: root.verticalCenter
      color: appConfig.fontColor2
      opacity: 0.7
      visible: formatType === TimeControl.FormatType.HoursMinutesSeconds

      font {
         family: appConfig.bodyFont.family
         pixelSize: 16
      }
   }

   Text {
      id: minutes
      x: 41
      width: 23
      height: 16
      anchors.verticalCenter: root.verticalCenter
      horizontalAlignment: Text.AlignRight
      color: appConfig.fontColor2
      visible: formatType === TimeControl.FormatType.HoursMinutesSeconds

      font {
         family: appConfig.bodyFont.family
         pixelSize: 16
      }
   }

   Text {
      id: minutesSuffix
      text: "m"
      x: minutes.x + minutes.width
      width: 14
      height: 16
      anchors.verticalCenter: root.verticalCenter
      color: appConfig.fontColor2
      opacity: 0.7
      visible: formatType === TimeControl.FormatType.HoursMinutesSeconds

      font {
         family: appConfig.bodyFont.family
         pixelSize: 16
      }
   }

   Text {
      id: seconds
      x: formatType === TimeControl.FormatType.HoursMinutesSeconds ? 78 : 8
      width: formatType === TimeControl.FormatType.HoursMinutesSeconds ? 23 : 70
      height: 16
      anchors.verticalCenter: root.verticalCenter
      horizontalAlignment: Text.AlignRight
      color: appConfig.fontColor2

      font {
         family: appConfig.bodyFont.family
         pixelSize: 16
      }
   }

   Text {
      id: secondsSuffix
      text: "s"
      x: seconds.x + seconds.width
      width: 9
      height: 16
      anchors.verticalCenter: root.verticalCenter
      color: appConfig.fontColor2
      opacity: 0.7

      font {
         family: appConfig.bodyFont.family
         pixelSize: 16
      }
   }

   Rectangle {
      id: separator
      x: root.width - downArrow.width - 1
      width: 1
      height: root.height
      color: appConfig.backgroundColor1
   }

   Text {
      id: downArrow
      x: root.width - width
      width: 16
      anchors.verticalCenter: root.verticalCenter
      color: appConfig.fontColor2
      text: String.fromCharCode(IconCode.SMALL_ARROW_DOWN)

      font {
         family: appConfig.iconFont.family
         pixelSize: 16
      }

      MouseArea {
         id: mouseArea
         anchors.fill: parent
         acceptedButtons: Qt.LeftButton
         onClicked: menu.open()

         Menu {
            id: menu
            x: downArrow.x - menu.width
            y: root.height

            RadioButton {
               id: hoursMinutesSecondsMenuItem
               text: qsTr("Hours Minutes Seconds")
               checked: formatType === TimeControl.FormatType.HoursMinutesSeconds

               onClicked: {
                  if (formatType !==  TimeControl.FormatType.HoursMinutesSeconds) {
                     formatType = TimeControl.FormatType.HoursMinutesSeconds
                  }
                  menu.close()
               }
            }

            RadioButton {
               id: secondsMenuItem
               text: qsTr("Seconds")
               checked: formatType === TimeControl.FormatType.Seconds

               onClicked: {
                  if (formatType !==  TimeControl.FormatType.Seconds) {
                     formatType = TimeControl.FormatType.Seconds
                  }
                  menu.close()
               }
            }
         }
      }
   }

   Component.onCompleted: {
      display.groupSeparator = Qt.locale().groupSeparator
   }
}
