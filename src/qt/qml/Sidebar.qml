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
      }
   }

   ListView {
      anchors.fill: parent
      model: trackControlPanelsModel
      delegate: trackControlPanelsDelegate
   }
}
