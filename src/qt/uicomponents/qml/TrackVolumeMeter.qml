import QtQuick

import Audacity.UiComponents

Item {
   id: root
   width: implicitWidth
   height: implicitHeight
   implicitWidth: 24
   implicitHeight: 124
   objectName: "TrackVolumeMeter"

   enum Channels {
      Mono,
      Stereo
   }

   property var channels: Stereo

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
         when: channels === TrackVolumeMeter.Mono
         PropertyChanges { target: meterPanels[0]; x: 8; height: 8 }
         PropertyChanges { target: meterPanels[1]; visible: false }
      },
      State {
         name: "STEREO"
         when: channels === TrackVolumeMeter.Stereo
         PropertyChanges { target: meterPanels[0]; x: 4; height: 7 }
         PropertyChanges { target: meterPanels[1]; visible: true }
      }
   ]
}
