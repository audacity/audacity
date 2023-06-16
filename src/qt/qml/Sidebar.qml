import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Qt.labs.qmlmodels

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

      if (type === "label") {
         label = qsTr("Label track %1").arg(nextTrack.label)
         trackControlPanelsModel.append({ "type": type, "label": label })
         nextTrack.label += 1
      } else if (type === "stereo") {
         label = qsTr("Stereo track %1").arg(nextTrack.stereo)
         trackControlPanelsModel.append({ "type": type, "label": label, "isMuted": false, "isSoloed": false })
         nextTrack.stereo += 1
      } else if (type == "mono") {
         label = qsTr("Mono track %1").arg(nextTrack.mono)
         trackControlPanelsModel.append({ "type": type, "label": label, "isMuted": false, "isSoloed": false })
         nextTrack.mono += 1
      } else if (type == "video") {
         label = qsTr("Video track %1").arg(nextTrack.video)
         trackControlPanelsModel.append({ "type": type, "label": label, "isMuted": false, "isSoloed": false })
         nextTrack.video += 1
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
      color: appConfig.strokeColor
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
         var index = Math.floor(Math.random() * trackControlPanelsChooser.choices.length)
         var trackType = trackControlPanelsChooser.choices[index].roleValue

         addTrackControlPanel(trackType)
      }

      Rectangle {
         id: separator
         y: parent.height - 1
         width: parent.width
         height: 1
         color: appConfig.strokeColor
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
      addTrackControlPanel("label")
      addTrackControlPanel("stereo")
      addTrackControlPanel("mono")
      addTrackControlPanel("video")
   }
}
