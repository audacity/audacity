import QtQuick
import QtQuick.Controls

import Audacity
import Audacity.UiComponents
import Audacity.Ui

Popup {
   id: root
   implicitWidth: 331
   implicitHeight: 212
   padding: 0
   modal: true
   focus: true
   closePolicy: Popup.CloseOnEscape | Popup.CloseOnPressOutside
   objectName: "AddNewTrackPanel"

   signal createTracks(type: string, quantity: int)

   QtObject {
      id: prv
      property string type
      property string numberOfTracksPrompt: numberOfTracks
      readonly property string numberOfTracks: qsTr("Number of tracks")
      property bool validNumberOfTracks: true
   }

   onAboutToShow: {
      prv.numberOfTracksPrompt = prv.numberOfTracks
      input.text = numberOfTracksValidator.bottom
      input.focus = false
   }

   MouseArea {
      id: mouseArea
      anchors.fill: parent

      onClicked: {
         input.focus = false
      }
   }

   FlatButton {
      id: mono
      x: 12
      y: 20
      width: 97
      height: 64
      text: qsTr("Mono")
      icon: IconCode.MICROPHONE
      accentButton: true

      onClicked: {
         input.focus = false
         stereo.accentButton = false
         label.accentButton = false
         accentButton = true
         prv.type = "mono"
      }

      Component.onCompleted: {
         prv.type = "mono"
      }
   }

   FlatButton {
      id: stereo
      x: 117
      y: 20
      width: 97
      height: 64
      text: qsTr("Stereo")
      icon: IconCode.MICROPHONE
      accentButton: false

      onClicked: {
         input.focus = false
         mono.accentButton = false
         label.accentButton = false
         accentButton = true
         prv.type = "stereo"
      }
   }

   FlatButton {
      id: label
      x: 222
      y: 20
      width: 97
      height: 64
      text: qsTr("Label")
      icon: IconCode.FLAG
      accentButton: false

      onClicked: {
         input.focus = false
         mono.accentButton = false
         stereo.accentButton = false
         accentButton = true
         prv.type = "label"
      }
   }

   Text {
      id: numberOfTracksLabel
      y: 96
      height: 16
      text: prv.numberOfTracksPrompt
      color: ui.theme.fontColor1
      font.family: ui.theme.bodyFont.family
      font.pixelSize: 12
      anchors.left: mono.left
      anchors.right: label.right
      verticalAlignment: Text.AlignVCenter
      horizontalAlignment: Text.AlignHCenter
   }

   FlatButton {
      id: subtract
      x: 94
      y: 120
      width: 28
      height: 28
      text: "-"

      onClicked: {
         if (input.text === "") {
            input.focus = false
            return
         }

         var value = Number(input.text)
         if (value > numberOfTracksValidator.top) {
            input.text = numberOfTracksValidator.top
         } else if (value > numberOfTracksValidator.bottom) {
            input.text = value - 1
         }

         input.focus = false
      }
   }

   Rectangle {
      id: numberOfTracks
      x: 126
      y: 120
      width: 79
      height: 28
      color: ui.theme.textFieldColor
      radius: 3
      smooth: true
      clip: true
      border.color: prv.validNumberOfTracks
         ? (input.focus ? ui.theme.accentColor : ui.theme.strokeColor2)
         : (input.focus ? ui.theme.recordColor : ui.theme.invalidInputColor)

      TextInput {
         id: input
         anchors.fill: parent
         anchors.margins: 2
         font.family: ui.theme.bodyFont.family
         font.pixelSize: 12
         color: ui.theme.fontColor1
         selectedTextColor: ui.theme.fontColor1
         selectionColor: ui.theme.textHighlightColor
         clip: true
         cursorVisible: focus && selectedText.length === 0
         text: numberOfTracksValidator.bottom
         horizontalAlignment: Qt.AlignHCenter
         verticalAlignment: Qt.AlignVCenter
         inputMethodHints: Qt.ImhFormattedNumbersOnly
         selectByMouse: true

         validator: IntValidator {
            id: numberOfTracksValidator
            bottom: 1
            top: 25
         }

         onTextChanged: {
            prv.validNumberOfTracks = (text === "" || acceptableInput)
            if (prv.validNumberOfTracks === true) {
               prv.numberOfTracksPrompt = prv.numberOfTracks
            }

            if (text === "") {
               return
            }

            if (acceptableInput === false) {
               prv.numberOfTracksPrompt =
                  qsTr("Enter a value between %1-%2")
                     .arg(numberOfTracksValidator.bottom)
                     .arg(numberOfTracksValidator.top)
            }
         }

         onFocusChanged: {
            if (focus) {
               selectAll()
            } else {
               deselect()
            }

            if (text === "") {
               text = numberOfTracksValidator.bottom
            }
         }
      }
   }

   FlatButton {
      id: add
      x: 209
      y: 120
      width: 28
      height: 28
      text: "+"

      onClicked: {
         if (input.text === "") {
            input.focus = false
            return
         }

         var value = Number(input.text)
         if (value < numberOfTracksValidator.bottom) {
            input.text = numberOfTracksValidator.bottom
         } else if (value < numberOfTracksValidator.top) {
            input.text = value + 1
         }

         input.focus = false
      }
   }

   FlatButton {
      id: create
      text: qsTr("Create")
      x: (root.width - width) / 2
      y: separator.y + (root.height - separator.y - height) / 2
      width: 307
      height: 28
      accentButton: true
      enabled: prv.validNumberOfTracks

      onClicked: {
         input.focus = false
         root.createTracks(prv.type, Number(input.text))
      }
   }

   background: Rectangle {
      y: 8
      width: root.width
      height: root.height - 8
      color: ui.theme.backgroundColor2
      radius: 4

      border {
         color: ui.theme.strokeColor2
         width: 1
      }
   }

   Canvas {
      id: canvas
      x: 131
      width: 15
      height: 9
      antialiasing: true

      onPaint: {
         var context = canvas.context
         if (!context) {
            context = getContext("2d")
            context.lineCap = "squared"
         }

         context.lineWidth = 2
         context.strokeStyle = ui.theme.strokeColor2
         context.beginPath()
         context.moveTo(0, 9)
         context.lineTo(7, 0)
         context.lineTo(14, 9)
         context.stroke()

         context.fillStyle = ui.theme.backgroundColor2
         context.fill()
      }
   }

   Rectangle {
      id: separator
      y: root.height - 52
      width: parent.width
      height: 1
      color: ui.theme.strokeColor1
      opacity: ui.theme.opacityLight
   }
}
