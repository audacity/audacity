import QtQuick
import QtQuick.Layouts

import Audacity
import Audacity.UiComponents

Item {
   id: root
   objectName: "TrackControlPanel"
   height: row.height + separator.height

   property var trackType: TrackType.Mono
   property alias name: trackControlBody.label
   property alias muted: trackControlBody.muted
   property alias soloed: trackControlBody.soloed

   signal optionsClicked()
   signal muteClicked()
   signal soloClicked()
   signal addClicked()

   Rectangle {
      id: background
      anchors.fill: parent
      color: appConfig.backgroundColor2
      opacity: 1.0

      states: [
         State {
            name: "HOVERED"
            when: mouseArea.containsMouse

            PropertyChanges {
               target: background
               color: appConfig.backgroundColor1
               opacity: appConfig.buttonOpacityHover
            }
         }
      ]
   }

   MouseArea {
      id: mouseArea
      anchors.fill: parent
      hoverEnabled: true

      Row {
         id: row
         spacing: 0

         TrackControlBody {
            id: trackControlBody
            trackType: root.trackType
            onOptionsClicked: root.optionsClicked()
            onMuteClicked: root.muteClicked()
            onSoloClicked: root.soloClicked()
            onAddClicked: root.addClicked()
         }

         TrackVolumeMeter {
            id: trackVolumeMeter
            trackType: root.trackType
         }

         Component.onCompleted: {
            row.height = Math.max(trackControlBody.height, trackVolumeMeter.height)
         }
      }
   }

   Rectangle {
      id: separator
      y: row.height
      height: 2
      width: root.width
      color: appConfig.strokeColor1
   }
}
