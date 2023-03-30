import QtQuick

import Audacity
import Audacity.UiComponents

Item {
   id: root
   width: 24
   objectName: "TrackVolumeMeter"

   property var trackType: undefined

   property list<MeterPanel> meterPanels: [
      MeterPanel {
         y: root.height - 6
         width: root.height - 14
         transformOrigin: Item.TopLeft
         rotation: 270
      },
      MeterPanel {
         x: 13
         y: root.height - 6
         width: root.height - 14
         height: 7
         transformOrigin: Item.TopLeft
         rotation: 270
      }
   ]

   children: meterPanels

   states: [
      State {
         name: "MONO"
         when: trackType === TrackType.Mono
         PropertyChanges { target: root; height: 124; visible: true }
         PropertyChanges { target: meterPanels[0]; x: 8; height: 8 }
         PropertyChanges { target: meterPanels[1]; visible: false }
      },
      State {
         name: "STEREO"
         when: trackType === TrackType.Stereo
         PropertyChanges { target: root; height: 124; visible: true }
         PropertyChanges { target: meterPanels[0]; x: 4; height: 7 }
         PropertyChanges { target: meterPanels[1]; visible: true }
      },
      State {
         name: "VIDEO"
         when: trackType === TrackType.Video
         PropertyChanges { target: root; height: 84; visible: true }
         PropertyChanges { target: meterPanels[0]; x: 4; height:7 }
         PropertyChanges { target: meterPanels[1]; visible: true }
      },
      State {
         name: "LABEL"
         when: trackType === TrackType.Label
         PropertyChanges { target: root; height: 84; visible: false }
      }
   ]
}
