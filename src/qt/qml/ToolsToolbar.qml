import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Audacity
import Audacity.UiComponents

Rectangle {
   id: root
   height: 48
   width: parent.width
   color: appConfig.backgroundColor1

   property var workspaceMode: Workspace.Mode.Classic
   property alias enableVolumeTester: masterVolumeToolbar.testerVisible

   signal playbackStarted()
   signal playbackStopped()
   signal playbackPaused()
   signal updateStatusBar(status: string)

   onWorkspaceModeChanged: {
      transportToolbar.workspaceMode = workspaceMode
      editToolbar.workspaceMode = workspaceMode
   }

   ToolsToolbarHandler {
      id: toolbarHandler

      onUpdateStatusBar: function(status) {
         root.updateStatusBar(status)
      }
   }

   Flow {
      id: flowId
      spacing: 0
      Layout.alignment: Qt.AlignVCenter
      anchors.fill: parent

      TransportToolbar {
         id: transportToolbar
         height: root.height
         gripVisible: Positioner.isFirstItem
         separatorVisible: !Positioner.isLastItem

         onPlaybackStarted: {
            timeToolbar.start()
            root.playbackStarted()
            masterVolumeToolbar.startDemo()
         }

         onPlaybackStopped: {
            timeToolbar.stop()
            root.playbackStopped()
            masterVolumeToolbar.stopDemo()
         }

         onPlaybackPaused:  {
            timeToolbar.pause()
            root.playbackPaused()
            masterVolumeToolbar.pauseDemo()
         }

         onUpdateStatusBar: function(status) {
            root.updateStatusBar(status)
         }
      }

      EditToolbar {
         id: editToolbar
         height: root.height
         gripVisible:  Positioner.isFirstItem
         separatorVisible: !Positioner.isLastItem

         onUpdateStatusBar: function(status) {
            root.updateStatusBar(status)
         }
      }

      TimeToolbar {
         id: timeToolbar
         height: root.height
         gripVisible:  Positioner.isFirstItem
         separatorVisible: !Positioner.isLastItem

         onUpdateStatusBar: function(status) {
            root.updateStatusBar(status)
         }
      }

      RowLayout {
         spacing: 8
         height: parent.height

         MasterVolumeToolbar {
            id: masterVolumeToolbar
            height: root.height
            width: 200
            gripVisible:  Positioner.isFirstItem
            separatorVisible: !Positioner.isLastItem

            onUpdateStatusBar: function(status) {
               root.updateStatusBar(status)
            }
         }

         FlatButton {
            id: setup
            x: root.width - implicitWidth - 12
            icon: IconCode.SETUP
            onClicked: toolbarHandler.Setup()
         }
      }
   }

   Rectangle {
      anchors.top: root.bottom
      height: 1
      width: parent.width
      color: appConfig.strokeColor1
   }
}

