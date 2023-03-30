import QtQuick
import QtQuick.Controls

import Audacity
import Audacity.UiComponents

Item {
   id: root
   width: 256
   objectName: "TrackControlBody"

   property var trackType: undefined
   property string label: ""
   property bool muted: false
   property bool soloed: false

   signal optionsClicked()
   signal muteClicked()
   signal soloClicked()
   signal addClicked()

   Text {
      id: icon
      x: 12
      y: 14
      width: 16
      height: 16
      color: appConfig.fontColor1

      font {
         family: appConfig.iconFont.family
         pixelSize: 16
      }
   }

   Text {
      x: 36
      y: 14
      height: 16
      horizontalAlignment: Text.AlignLeft
      color: appConfig.fontColor1
      text: label

      font {
         family: appConfig.bodyFont.family
         pixelSize: 12
      }
   }

   FlatButton {
      id: options
      x: 224
      y: 12
      width: 20
      height: 20
      transparent: true
      icon: IconCode.MENU_THREE_DOTS
      onClicked: root.optionsClicked()
   }

   FlatButton {
      id: mute
      x: 12
      y: 48
      width: 20
      height: 20
      text: qsTr("M")
      textFont.pixelSize: 12
      accentButton: muted
      onClicked: root.muteClicked()
   }

   FlatButton {
      id: solo
      x: 36
      y: 48
      width: 20
      height: 20
      text: qsTr("S")
      textFont.pixelSize: 12
      accentButton: soloed
      onClicked: root.soloClicked()
   }

   StyledSlider {
      id: slider
      x: 64
      y: 50
      width: 142
      height: 16
   }

   StyledDial {
      id: dial
      x: 214
      y: 43
      width: 30
      height: 30
   }

   FlatButton {
      id: add
      x: 12
      y: 84
      width: 232
      height: 28
      textFont.pixelSize: 12
      onClicked: root.addClicked()
   }

   Rectangle {
      id: separator
      x: root.width - separator.width
      width: 1
      height: root.height
      color: appConfig.strokeColor1
   }

   states: [
      State {
         name: "MONO"
         when: trackType === TrackType.Mono
         PropertyChanges { target: root; height: 124 }
         PropertyChanges { target: icon; text: String.fromCharCode(IconCode.MICROPHONE) }
         PropertyChanges { target: mute; visible: true }
         PropertyChanges { target: solo; visible: true }
         PropertyChanges { target: slider; visible: true }
         PropertyChanges { target: dial; visible: true }
         PropertyChanges { target: add; y: 84; visible: true; text: qsTr("Add effects") }
      },
      State {
         name: "STEREO"
         when: trackType === TrackType.Stereo
         PropertyChanges { target: root; height: 124 }
         PropertyChanges { target: icon; text: String.fromCharCode(IconCode.MICROPHONE) }
         PropertyChanges { target: mute; visible: true }
         PropertyChanges { target: solo; visible: true }
         PropertyChanges { target: slider; visible: true }
         PropertyChanges { target: dial; visible: true }
         PropertyChanges { target: add; y: 84; visible: true; text: qsTr("Add effects") }
      },
      State {
         name: "VIDEO"
         when: trackType === TrackType.Video
         PropertyChanges { target: root; height: 84 }
         PropertyChanges { target: icon; text: String.fromCharCode(IconCode.TV) }
         PropertyChanges { target: mute; visible: true }
         PropertyChanges { target: solo; visible: true }
         PropertyChanges { target: slider; visible: true }
         PropertyChanges { target: dial; visible: true }
         PropertyChanges { target: add; visible: false }
      },
      State {
         name: "LABEL"
         when: trackType === TrackType.Label
         PropertyChanges { target: root; height: 84 }
         PropertyChanges { target: icon; text: String.fromCharCode(IconCode.FLAG) }
         PropertyChanges { target: mute; visible: false }
         PropertyChanges { target: solo; visible: false }
         PropertyChanges { target: slider; visible: false }
         PropertyChanges { target: dial; visible: false }
         PropertyChanges { target: add; y: 44; visible: true; text: qsTr("Add marker") }
      }
   ]
}
