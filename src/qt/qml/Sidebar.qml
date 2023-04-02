import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Audacity
import Audacity.UiComponents

Rectangle {
   id: root
   width: 280
   color: appConfig.backgroundColor2
   objectName: "Sidebar"

   QtObject {
      id: nextTrack
      property int label: 1
      property int stereo: 1
      property int mono: 1
      property int video: 1
   }

   signal updateStatusBar(status: string)

   function addTrackControlPanel(type) {
      var label

      if (type === TrackType.Label) {
         label = qsTr("Label track %1").arg(nextTrack.label)
         nextTrack.label += 1
      } else if (type === TrackType.Stereo) {
         label = qsTr("Stereo track %1").arg(nextTrack.stereo)
         nextTrack.stereo += 1
      } else if (type == TrackType.Mono) {
         label = qsTr("Mono track %1").arg(nextTrack.mono)
         nextTrack.mono += 1
      } else if (type == TrackType.Video) {
         label = qsTr("Video track %1").arg(nextTrack.video)
         nextTrack.video += 1
      } else {
         console.log("Invalid Track Type:", type)
         return
      }

      trackControlPanelsModel.append({ "type": type, "label": label, "isMuted": false, "isSoloed": false })
      root.updateStatusBar("Added '%1'".arg(label))
   }

   Rectangle {
      id: verticalSeparator
      x: parent.width
      width: 1
      height: parent.height
      color: appConfig.strokeColor1
   }

   FlatButton {
      id: button
      width: root.width
      height: 28
      radius: 0
      transparent: true
      textFont.pixelSize: 12
      text: qsTr("+ Add new track")

      onClicked: {
         var trackType = Math.floor(Math.random() * 4)
         addTrackControlPanel(trackType)
      }

      Rectangle {
         id: separator
         y: parent.height - 1
         width: parent.width
         height: 1
         color: appConfig.strokeColor1
      }
   }


   ListModel {
      id: trackControlPanelsModel
   }

   Component {
      id: trackControlPanelsDelegate

      TrackControlPanel {
         width: root.width
         trackType: type
         name: label
         muted: isMuted
         soloed: isSoloed

         onOptionsClicked: {
            var track = trackControlPanelsModel.get(index).label
            trackControlPanelsModel.remove(index)

            root.updateStatusBar("Removed '%1'".arg(track))
         }

         onMuteClicked: {
            var isMuted = trackControlPanelsModel.get(index).isMuted
            trackControlPanelsModel.setProperty(index, "isMuted", !isMuted)

            root.updateStatusBar("%1 mute button clicked".arg(name))
         }

         onSoloClicked: {
            var isSoloed = trackControlPanelsModel.get(index).isSoloed
            trackControlPanelsModel.setProperty(index, "isSoloed", !isSoloed)

            root.updateStatusBar("%1 solo button clicked".arg(name))
         }

         onAddClicked: root.updateStatusBar("%1 add button clicked".arg(name))
      }
   }

   ListView {
      id: trackControlPanelsView
      y: button.height
      width: root.width
      height: root.height - button.height
      clip: true
      model: trackControlPanelsModel
      delegate: trackControlPanelsDelegate
   }

   Component.onCompleted: {
      addTrackControlPanel(TrackType.Label)
      addTrackControlPanel(TrackType.Stereo)
      addTrackControlPanel(TrackType.Mono)
      addTrackControlPanel(TrackType.Video)
   }
}
