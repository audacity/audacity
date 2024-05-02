import QtQuick
import QtQuick.Controls

import Audacity
import Audacity.UiComponents
import Audacity.Ui

Item {
   id: root
   width: implicitWidth
   height: implicitHeight
   implicitHeight: 28
   objectName: "TimeControl"

   property alias formatType: display.formatType
   property color iconColor: ui.theme.fontColor1
   property int icon: IconCode.TIMECODE

   enum FormatType {
      Seconds,
      HoursMinutesSeconds
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

   Rectangle {
      id: displayPart
      height: parent.height
      width: root.width - 17
      radius: 3
      color: ui.theme.backgroundColor4

      QtObject {
         id: display

         onValueChanged: update()
         onFormatTypeChanged: update()

         property var formatType: TimeControl.FormatType.HoursMinutesSeconds
         property var groupSeparator: ' '
         property int value: 0

         function valueInSecondsFormat() {
            return String(display.value).padStart(6, '0')
         }

         function valueInHoursMinutesSecondsFormat() {
            var hrs = Math.floor(display.value / 3600)
            var mins = Math.floor((display.value - hrs * 3600) / 60)
            var secs = display.value - mins * 60 - hrs * 3600

            return String(hrs).padStart(2, '0')
               + String(mins).padStart(2, '0')
               + String(secs).padStart(2, '0')
         }

         function update() {
            var text = ""

            if (formatType === TimeControl.FormatType.Seconds) {
               text = valueInSecondsFormat()
            } else if (formatType === TimeControl.FormatType.HoursMinutesSeconds) {
               text = valueInHoursMinutesSecondsFormat()
            }

            for (var i = 0; i < text.length; i++) {
               displayPart.digits[i].text = text[i]
            }
         }
      }

      property list<Text> digits: [
         Text {
            x: 8
            height: root.height
            verticalAlignment: Text.AlignVCenter
            horizontalAlignment: Text.AlignHCenter
            width: 12
            color: ui.theme.fontColor2
            font: ui.theme.timecodeFont
         },
         Text {
            x: 20
            height: root.height
            verticalAlignment: Text.AlignVCenter
            horizontalAlignment: Text.AlignHCenter
            width: 12
            color: ui.theme.fontColor2
            font: ui.theme.timecodeFont
         },
         Text {
            width: 12
            height: root.height
            verticalAlignment: Text.AlignVCenter
            horizontalAlignment: Text.AlignHCenter
            color: ui.theme.fontColor2
            font: ui.theme.timecodeFont
         },
         Text {
            width: 12
            height: root.height
            verticalAlignment: Text.AlignVCenter
            horizontalAlignment: Text.AlignHCenter
            color: ui.theme.fontColor2
            font: ui.theme.timecodeFont
         },
         Text {
            width: 12
            height: root.height
            verticalAlignment: Text.AlignVCenter
            horizontalAlignment: Text.AlignHCenter
            color: ui.theme.fontColor2
            font: ui.theme.timecodeFont
         },
         Text {
            width: 12
            height: root.height
            verticalAlignment: Text.AlignVCenter
            horizontalAlignment: Text.AlignHCenter
            color: ui.theme.fontColor2
            font: ui.theme.timecodeFont
         }
      ]

      Text {
         id: separator
         text:  display.groupSeparator
         x: 44
         width: 4
         height: root.height
         verticalAlignment: Text.AlignVCenter
         color: ui.theme.fontColor2
         font: ui.theme.bodyFont.family
      }

      Text {
         id: hours
         text: "h"
         width: 10
         height: root.height
         verticalAlignment: Text.AlignVCenter
         color: ui.theme.fontColor2
         font: ui.theme.bodyFont.family
         opacity: ui.theme.opacityStrong
      }

      Text {
         id: minutes
         text: "m"
         width: 14
         height: root.height
         verticalAlignment: Text.AlignVCenter
         color: ui.theme.fontColor2
         font: ui.theme.bodyFont.family
         opacity: ui.theme.opacityStrong
      }

      Text {
         id: seconds
         text: "s"
         width: 9
         height: root.height
         verticalAlignment: Text.AlignVCenter
         color: ui.theme.fontColor2
         font: ui.theme.bodyFont.family
         opacity: ui.theme.opacityStrong
      }

      Rectangle {
         x: displayPart.width - width
         width: displayPart.radius
         height: displayPart.height
         color: ui.theme.backgroundColor4
      }

      Component.onCompleted: {
         display.groupSeparator = Qt.locale().groupSeparator

         for (var i = 0; i < digits.length; i++) {
            displayPart.children.push(digits[i])
         }
      }

      states: [
         State {
            name: "Seconds"
            when: formatType === TimeControl.FormatType.Seconds
            PropertyChanges { target: root; implicitWidth: 121 }
            PropertyChanges { target: hours; visible: false }
            PropertyChanges { target: minutes; visible: false }
            PropertyChanges { target: seconds; x: 86; visible: true }
            PropertyChanges { target: separator; visible: true }
            PropertyChanges { target: displayPart.digits[2]; x: 32 }
            PropertyChanges { target: displayPart.digits[3]; x: 48 }
            PropertyChanges { target: displayPart.digits[4]; x: 60 }
            PropertyChanges { target: displayPart.digits[5]; x: 72 }
         },
         State {
            name: "HoursMinutesSeconds"
            when: formatType === TimeControl.FormatType.HoursMinutesSeconds
            PropertyChanges { target: root; implicitWidth: 145 }
            PropertyChanges { target: hours; x:33; visible: true }
            PropertyChanges { target: minutes; x: 69; visible: true }
            PropertyChanges { target: seconds; x: 110; visible: true }
            PropertyChanges { target: separator; visible: false }
            PropertyChanges { target: displayPart.digits[2]; x: 44 }
            PropertyChanges { target: displayPart.digits[3]; x: 56 }
            PropertyChanges { target: displayPart.digits[4]; x: 84 }
            PropertyChanges { target: displayPart.digits[5]; x: 96 }
         }
      ]
   }

   Rectangle {
      id: buttonPart
      x: root.width - buttonPart.width
      radius: 3
      width: 16
      height: root.height
      color: ui.theme.backgroundColor4

      states: [
         State {
            name: "HOVER"
            when: mouseArea.containsMouse && !mouseArea.pressed && !menu.visible

            PropertyChanges {
               target: buttonPart
               color: ui.theme.backgroundColor3
               opacity: ui.theme.opacityOpaque
            }

            PropertyChanges {
               target: buttonPartLeftEdge
               color: ui.theme.backgroundColor3
               opacity: opacityOpaque
            }
         },

         State {
            name: "PRESSED"
            when: mouseArea.pressed && !menu.visible

            PropertyChanges {
               target: buttonPart
               color: ui.theme.backgroundColor3
               opacity: opacityMedium
            }

            PropertyChanges {
               target: buttonPartLeftEdge
               color: ui.theme.backgroundColor3
               opacity: opacityMedium
            }
         },

         State {
            name: "ACTIVE"
            when: menu.visible

            PropertyChanges {
               target: buttonPart
               color: "black"
               opacity: opacityOpaque
            }

            PropertyChanges {
               target: buttonPartLeftEdge
               color: "black"
               opacity: opacityOpaque
            }
         }
      ]

      Rectangle {
         id: buttonPartLeftEdge
         width: buttonPart.radius
         height: buttonPart.height
         color: ui.theme.backgroundColor4
      }

      Text {
         id: downArrow
         anchors.centerIn: buttonPart
         anchors.fill: buttonPart
         verticalAlignment: Text.AlignVCenter
         horizontalAlignment: Text.AlignHCenter
         color: ui.theme.fontColor2
         text: String.fromCharCode(IconCode.SMALL_ARROW_DOWN)

         font {
            family: ui.theme.iconFont.family
            pixelSize: 16
         }
      }

      MouseArea {
         id: mouseArea
         anchors.fill: parent
         acceptedButtons: Qt.LeftButton
         hoverEnabled: true
         onClicked: menu.open()

         Menu {
            id: menu
            x: buttonPart.x - menu.width
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
}
