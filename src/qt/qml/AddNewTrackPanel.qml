import QtQuick
import QtQuick.Controls

import Audacity
import Audacity.UiComponents
import Audacity.UiThemes

Popup {
   id: root
   implicitWidth: 324
   implicitHeight: 204
   padding: 0
   modal: true
   focus: true
   closePolicy: Popup.CloseOnEscape | Popup.CloseOnPressOutside | Popup.CloseOnPressOutsideParent
   objectName: "AddNewTrackPanel"

   signal createTracks(type: string, quantity: int)

   QtObject {
      id: prv
      property string type
      property bool validNumberOfTracks: true
   }

   onAboutToShow: {
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
      y: 12
      width: 96
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
      x: 114
      y: 12
      width: 96
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
      x: 216
      y: 12
      width: 96
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
      y: 88
      height: 16
      text: qsTr("Number of tracks")
      color: UiTheme.fontColor1
      font.pixelSize: 12
      font.family: appConfig.bodyFont.family
      anchors.left: mono.left
      anchors.right: label.right
      verticalAlignment: Text.AlignVCenter
      horizontalAlignment: Text.AlignHCenter
   }

   FlatButton {
      id: subtract
      x: 106
      y: 112
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
      x: 138
      y: 112
      width: 48
      height: 28
      color: UiTheme.textFieldColor
      radius: 3
      smooth: true
      clip: true
      border.color: prv.validNumberOfTracks
         ? (input.focus ? UiTheme.accentColor : UiTheme.strokeColor)
         : (input.focus ? UiTheme.recordColor : UiTheme.invalidInputColor)

      TextInput {
         id: input
         anchors.fill: parent
         anchors.margins: 2
         font.family: appConfig.bodyFont.family
         font.pixelSize: 12
         color: UiTheme.fontColor1
         selectedTextColor: UiTheme.fontColor1
         selectionColor: UiTheme.textHighlightColor
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
               numberOfTracksLabel.text = qsTr("Number of tracks")
            }

            if (text === "") {
               return
            }

            if (acceptableInput === false) {
               numberOfTracksLabel.text =
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
      x: 190
      y: 112
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
      width: 288
      height: 28
      accentButton: true
      enabled: prv.validNumberOfTracks

      onClicked: {
         input.focus = false
         root.createTracks(prv.type, Number(input.text))
      }
   }

   background: Rectangle {
      anchors.fill: parent
      color: UiTheme.backgroundColor2
      radius: 4

      border {
         color: UiTheme.strokeColor
         width: 1
      }

      Rectangle {
         id: separator
         y: root.height - 52
         width: parent.width
         height: 1
         color: UiTheme.strokeColor
      }
   }
}
