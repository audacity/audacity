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

   Column {
      spacing: 0

      TrackControlPanel {
         id: trackControlPanel1
         width: root.width
         trackType: TrackType.Label
         name: qsTr("Label track 1")
         muted: false
         soloed: false
      }

      TrackControlPanel {
         id: trackControlPanel2
         width: root.width
         trackType: TrackType.Stereo
         name: qsTr("Stereo track 1")
         muted: false
         soloed: false
      }

      TrackControlPanel {
         id: trackControlPanel3
         width: root.width
         trackType: TrackType.Mono
         name: qsTr("Mono track 1")
         muted: false
         soloed: false
      }

      TrackControlPanel {
         id: trackControlPanel4
         width: root.width
         trackType: TrackType.Video
         name: qsTr("Video track 1")
         muted: false
         soloed: false
      }
   }
}
