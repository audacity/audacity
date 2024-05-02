import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Qt.labs.qmlmodels

import Audacity
import Audacity.UiComponents
import Audacity.Ui

Rectangle {
   id: root
   width: 280
   color: ui.theme.backgroundColor2
   objectName: "Sidebar"

   QtObject {
      id: nextTrack
      property int label: 1
      property int stereo: 1
      property int mono: 1
      property int video: 1
   }

   signal updateStatusBar(status: string)

   function addTrackControlPanel(type, quantity) {
      var label

      if (type === "label") {
         for (var i = 0; i < quantity; i++) {
            label = qsTr("Label track %1").arg(nextTrack.label)
            trackControlPanelsModel.append({ "type": type, "label": label })
            nextTrack.label += 1
         }
      } else if (type === "stereo") {
         for (var i = 0; i < quantity; i++) {
            label = qsTr("Stereo track %1").arg(nextTrack.stereo)
            trackControlPanelsModel.append({ "type": type, "label": label, "isMuted": false, "isSoloed": false })
            nextTrack.stereo += 1
         }
      } else if (type == "mono") {
         for (var i = 0; i < quantity; i++) {
            label = qsTr("Mono track %1").arg(nextTrack.mono)
            trackControlPanelsModel.append({ "type": type, "label": label, "isMuted": false, "isSoloed": false })
            nextTrack.mono += 1
         }
      } else if (type == "video") {
         for (var i = 0; i < quantity; i++) {
            label = qsTr("Video track %1").arg(nextTrack.video)
            trackControlPanelsModel.append({ "type": type, "label": label, "isMuted": false, "isSoloed": false })
            nextTrack.video += 1
         }
      } else {
         console.log("Invalid Track Type:", type)
         return
      }

      root.updateStatusBar("Added '%1'".arg(label))
   }

   Rectangle {
      id: verticalSeparator
      x: parent.width
      width: 1
      height: parent.height
      color: ui.theme.strokeColor2
   }

   FlatButton {
      id: button
      width: root.width
      height: 32
      radius: 0
      transparent: true
      textFont.pixelSize: 12
      text: qsTr("+ Add new track")
      onClicked: addNewTrack.open()

      Rectangle {
         id: separator
         y: parent.height - 1
         width: parent.width
         height: 1
         color: ui.theme.strokeColor2
      }

      AddNewTrackPanel {
         id: addNewTrack
         x: 8
         y: parent.height

         onCreateTracks: (type, quantity) => {
            addTrackControlPanel(type, quantity)
            addNewTrack.close()
         }
      }
   }

   ListModel {
      id: trackControlPanelsModel
   }

   DelegateChooser {
      id: trackControlPanelsChooser
      role: "type"

      DelegateChoice {
         roleValue: "mono"

         MonoTrackControlPanel {
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

      DelegateChoice {
         roleValue: "stereo"

         StereoTrackControlPanel {
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

      DelegateChoice {
         roleValue: "video"

         VideoTrackControlPanel {
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
         }
      }

      DelegateChoice {
         roleValue: "label"

         LabelTrackControlPanel {
            name: label

            onOptionsClicked: {
               var track = trackControlPanelsModel.get(index).label
               trackControlPanelsModel.remove(index)

               root.updateStatusBar("Removed '%1'".arg(track))
            }

            onAddClicked: root.updateStatusBar("%1 add button clicked".arg(name))
         }
      }
   }

   ListView {
      id: trackControlPanelsView
      y: button.height
      width: root.width
      height: root.height - button.height
      clip: true
      model: trackControlPanelsModel
      delegate: trackControlPanelsChooser
   }

   Component.onCompleted: {
      addTrackControlPanel("label", 1)
      addTrackControlPanel("stereo", 1)
      addTrackControlPanel("mono", 1)
      addTrackControlPanel("video", 1)
   }
}
