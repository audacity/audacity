import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

import Audacity
import Audacity.UiComponents
import Audacity.Ui

import Audacity.TrackPanel

RowLayout {
   id: root
   height: 124
   width: 280
   spacing : 0

   property WaveTrackAdapter track

   signal optionsClicked()
   signal addClicked()

   
   Column
   {
      Layout.fillWidth : true
      Layout.fillHeight : true
      padding : 12
      spacing : 8

      RowLayout
      {
         width : parent.width - parent.padding * 2
         Text {
            id: icon
            color: ui.theme.fontColor1
            text: String.fromCharCode(IconCode.MICROPHONE)

            font {
               family: ui.theme.iconFont.family
               pixelSize: 16
            }
         }

         Text {
            Layout.fillWidth : true
            horizontalAlignment: Text.AlignLeft
            color: ui.theme.fontColor1
            text: track.name

            font {
               family: ui.theme.bodyFont.family
               pixelSize: 12
            }
         }

         FlatButton {
            id: options
            margins : 0
            transparent: true
            icon: IconCode.MENU_THREE_DOTS
            onClicked: root.optionsClicked()
         }
      }

      RowLayout
      {
         width : parent.width - parent.padding * 2

         FlatToggleButton {
            id: mute

            implicitWidth: 20
            implicitHeight: 20

            icon: IconCode.MUTE
            checked: track.mute

            onToggled: track.mute = !track.mute
         }

         FlatToggleButton {
            id: solo

            implicitWidth: 20
            implicitHeight: 20

            icon: IconCode.SOLO
            checked: track.solo

            onToggled: track.solo = !track.solo
         }

         StyledSlider {
            id: slider
            Layout.fillWidth : true
            implicitHeight: 16
            from : -1
            to : 1
            value : track.gain
            ToolTip.visible : pressed
            ToolTip.text : "Gain %1 dB".arg(value * 36.0)
         }

         StyledDial {
            id: dial
            implicitWidth: 30
            implicitHeight: 30
            value : track.pan
            ToolTip.visible : pressed
            ToolTip.text : "Pan: " + (value == 0 ? "Center" : "%1 %2".arg(value * 100.0).arg(value > 0 ? "Right" : "Left"))
         }
      }

      FlatButton {
         id: add
         width: parent.width - parent.padding * 2
         height: 28
         text: qsTr("Add effects")
         textFont.pixelSize: 12
         onClicked: root.addClicked()
      }
   }

   Rectangle {
      id: verticalSeparator
      implicitWidth: 1
      implicitHeight: parent.height
      color: ui.theme.strokeColor1
      opacity: ui.theme.opacityLight
   }

   TrackVolumeMeter {
      id: trackVolumeMeter
      implicitHeight: parent.height
      channels: track.channels == 1
                   ? TrackVolumeMeter.Channels.Mono
                   : TrackVolumeMeter.Channels.Stereo
      anchors.left: verticalSeparator.right
   }
}
