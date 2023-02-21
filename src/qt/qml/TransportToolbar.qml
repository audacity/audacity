import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Audacity
import Audacity.UiComponents

Item {
   id: root
   height: parent.height
   implicitHeight: height
   implicitWidth: contents.width

   objectName: "TransportToolbar"

   property bool isPlaying: false
   property bool gripVisible: false
   property bool separatorVisible: true

   property var workspaceMode: Workspace.Mode.Classic

   signal updateStatusBar(status: string)

   TransportToolbarHandler {
      id: toolbarHandler

      onUpdateStatusBar: function(status) {
         root.updateStatusBar(status)
      }

      onPlayStateChanged: function(isPlaying) {
         root.isPlaying = isPlaying
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
            color: appConfig.backgroundColor1
         }

         FlatButton {
            id: play
            icon: isPlaying ? IconCode.SOLID_PAUSE : IconCode.SOLID_PLAY
            iconColor: isPlaying ? "black" : appConfig.playColor
            onClicked: toolbarHandler.play()
         }

         FlatButton {
            id: stop
            icon: IconCode.SOLID_STOP
            onClicked: toolbarHandler.stop()
         }

         FlatButton {
            id: record
            icon: IconCode.SOLID_RECORD
            iconColor: appConfig.recordColor
            visible: root.workspaceMode === Workspace.Mode.Classic
            onClicked: toolbarHandler.record()
         }

         FlatButton {
            id: rewind
            icon: IconCode.SOLID_REWIND
            visible: root.workspaceMode !== Workspace.Mode.SimpleRecording
            onClicked: toolbarHandler.rewind()
         }

         FlatButton {
            id: fastForward
            icon: IconCode.SOLID_FAST_FORWARD
            visible: root.workspaceMode !== Workspace.Mode.SimpleRecording
            onClicked: toolbarHandler.fastForward()
         }

         FlatButton {
            id: record2
            icon: IconCode.SOLID_RECORD
            iconColor: appConfig.recordColor
            visible: root.workspaceMode !== Workspace.Mode.Classic
            onClicked: toolbarHandler.record()
         }

         FlatButton {
            id: loop
            icon: IconCode.LOOP
            visible: root.workspaceMode !== Workspace.Mode.SimpleRecording
            onClicked: toolbarHandler.loop()
         }
      }

      ToolbarSeparator {
         visible: separatorVisible
      }
   }
}
