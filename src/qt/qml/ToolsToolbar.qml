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

   property bool isPlaying: false
   property var workspaceMode: Workspace.Mode.Classic

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
      Layout.alignment: Qt.AlignHCenter
      anchors.fill: parent

      TransportToolbar {
         id: transportToolbar
         height: root.height
         gripVisible: Positioner.isFirstItem
         separatorVisible: !Positioner.isLastItem

         onUpdateStatusBar: function(status) {
            root.updateStatusBar(status)
         }
      }

      RowLayout {
         spacing: 8

         EditToolbar {
            id: editToolbar
            height: root.height
            gripVisible:  Positioner.isFirstItem
            separatorVisible: !Positioner.isLastItem
         }

         FlatButton {
            x: root.width - implicitWidth - 12
            id: setup
            icon: IconCode.SETUP
            onClicked: toolbarHandler.setup()
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

