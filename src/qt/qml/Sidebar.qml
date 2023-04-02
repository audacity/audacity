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

   signal updateStatusBar(status: string)

   Rectangle {
      id: verticalSeparator
      x: parent.width
      width: 1
      height: parent.height
      color: appConfig.strokeColor1
   }

   ListModel {
      id: trackControlPanelsModel

      ListElement { type: TrackType.Label; label: qsTr("Label track 1"); isMuted: false; isSoloed: false }
      ListElement { type: TrackType.Stereo; label: qsTr("Stereo track 1"); isMuted: false; isSoloed: false }
      ListElement { type: TrackType.Mono; label: qsTr("Mono track 1"); isMuted: false; isSoloed: false }
      ListElement { type: TrackType.Video; label: qsTr("Video track 1"); isMuted: false; isSoloed: false }
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
      anchors.fill: parent
      model: trackControlPanelsModel
      delegate: trackControlPanelsDelegate
   }
}
