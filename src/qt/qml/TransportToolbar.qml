import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Audacity
import Audacity.UiComponents
import Audacity.Ui

Item {
   id: root
   height: implicitHeight
   implicitHeight: 48
   implicitWidth: contents.width
   visible: toolbarHandler.toolbarVisible

   objectName: "TransportToolbar"

   property bool isPlaying: false
   property bool gripVisible: false
   property bool separatorVisible: true

   property var workspaceMode: Workspace.Mode.Classic

   signal playbackStarted()
   signal playbackStopped()
   signal playbackPaused()
   signal updateStatusBar(status: string)

   function registerToolbarConfiguration() {
      toolbarHandler.RegisterToolbarConfiguration()
   }

   TransportToolbarHandler {
      id: toolbarHandler

      onUpdateStatusBar: function(status) {
         root.updateStatusBar(status)
      }

      onPlayStateChanged: function(isPlaying) {
         if (isPlaying) {
            root.playbackStarted()
         } else {
            root.playbackPaused()
         }
         root.isPlaying = isPlaying
      }

      onPlaybackStopped: {
         root.playbackStopped()
      }
   }

   RowLayout {
      id: contents
      anchors.verticalCenter: parent.verticalCenter
      spacing: 8

      ToolbarGrip {
         id: grip
         visible: root.gripVisible
      }

      RowLayout {
         spacing: 2

         Rectangle {
            height: root.height
            width: root.gripVisible ? 2 : 6
            color: ui.theme.backgroundColor1
         }

         FlatButton {
            id: play
            icon: isPlaying ? IconCode.SOLID_PAUSE : IconCode.SOLID_PLAY
            iconColor: isPlaying ? ui.theme.fontColor1 : ui.theme.successColor
            iconSize: 14
            visible: toolbarHandler.playVisible
            onClicked: toolbarHandler.Play()
         }

         FlatButton {
            id: stop
            icon: IconCode.SOLID_STOP
            iconSize: 14
            visible: toolbarHandler.stopVisible
            onClicked: toolbarHandler.Stop()
         }

         FlatButton {
            id: record
            icon: IconCode.SOLID_RECORD
            iconColor: ui.theme.dangerColor
            iconSize: 14
            visible: toolbarHandler.recordVisible && root.workspaceMode === Workspace.Mode.Classic
            onClicked: toolbarHandler.Record()
         }

         FlatButton {
            id: rewind
            icon: IconCode.SOLID_REWIND
            iconSize: 14
            visible: toolbarHandler.rewindVisible && root.workspaceMode !== Workspace.Mode.SimpleRecording
            onClicked: toolbarHandler.Rewind()
         }

         FlatButton {
            id: fastForward
            icon: IconCode.SOLID_FAST_FORWARD
            iconSize: 14
            visible: toolbarHandler.fastForwardVisible && root.workspaceMode !== Workspace.Mode.SimpleRecording
            onClicked: toolbarHandler.FastForward()
         }

         FlatButton {
            id: record2
            icon: IconCode.SOLID_RECORD
            iconColor: ui.theme.dangerColor
            iconSize: 14
            visible: toolbarHandler.recordVisible && root.workspaceMode !== Workspace.Mode.Classic
            onClicked: toolbarHandler.Record()
         }

         FlatButton {
            id: loop
            icon: IconCode.LOOP
            iconSize: 14
            visible: toolbarHandler.loopVisible && root.workspaceMode !== Workspace.Mode.SimpleRecording
            onClicked: toolbarHandler.Loop()
         }
      }

      ToolbarSeparator {
         visible: separatorVisible
      }
   }

   Component.onCompleted: {
      toolbarHandler.StoreToolbarManager(ToolbarManager)
   }
}
