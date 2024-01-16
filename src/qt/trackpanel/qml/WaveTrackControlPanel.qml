import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

import Audacity
import Audacity.UiComponents
import Audacity.UiThemes

import Audacity.TrackPanel

Item {
   id: root
   height: 124
   width: 280

   property WaveTrackAdapter track

   signal optionsClicked()
   signal addClicked()

   Text {
      id: icon
      x: 12
      y: 14
      width: 16
      height: 16
      color: UiTheme.fontColor1
      text: String.fromCharCode(IconCode.MICROPHONE)

      font {
         family: UiTheme.iconFont.family
         pixelSize: 16
      }
   }

   Text {
      x: 36
      y: 14
      height: 16
      horizontalAlignment: Text.AlignLeft
      color: UiTheme.fontColor1
      text: track.name

      font {
         family: UiTheme.bodyFont.family
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
      accentButton: track.mute
      onClicked: track.mute = !track.mute
   }

   FlatButton {
      id: solo
      x: 36
      y: 48
      width: 20
      height: 20
      text: qsTr("S")
      textFont.pixelSize: 12
      accentButton: track.solo
      onClicked: track.solo = !track.solo
   }

   StyledSlider {
      id: slider
      x: 64
      y: 50
      width: 142
      height: 16
      from : -1
      to : 1
      value : track.gain
      ToolTip.visible : pressed
      ToolTip.text : "Gain %1 dB".arg(value * 36.0)
   }

   StyledDial {
      id: dial
      x: 214
      y: 43
      width: 30
      height: 30
      value : track.pan
      ToolTip.visible : pressed
      ToolTip.text : "Pan: " + (value == 0 ? "Center" : "%1 %2".arg(value * 100.0).arg(value > 0 ? "Right" : "Left"))
   }

   FlatButton {
      id: add
      x: 12
      y: 84
      width: 232
      height: 28
      text: qsTr("Add effects")
      textFont.pixelSize: 12
      onClicked: root.addClicked()
   }

   Rectangle {
      id: verticalSeparator
      x: 255
      width: 1
      height: root.height
      color: UiTheme.strokeColor1
      opacity: UiTheme.opacityLight
   }

   TrackVolumeMeter {
      id: trackVolumeMeter
      height: root.height// - horizontalSeparator.height
      channels: TrackVolumeMeter.Channels.Stereo
      anchors.left: verticalSeparator.right
   }
}
