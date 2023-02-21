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

   objectName: "EditToolbar"

   property bool gripVisible: false
   property bool separatorVisible: true

   property var workspaceMode: Workspace.Mode.Classic

   signal updateStatusBar(status: string)

   EditToolbarHandler {
      id: toolbarHandler

      onUpdateStatusBar: function(status) {
         root.updateStatusBar(status)
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
            id: automation
            icon: IconCode.AUTOMATION
            visible: root.workspaceMode === Workspace.Mode.Classic ||
                     root.workspaceMode === Workspace.Mode.AudioEditing
            onClicked: toolbarHandler.automation()
         }
      }

      RowLayout {
         spacing: 1
         FlatButton {
            id: zoomIn
            icon: IconCode.ZOOM_IN
            onClicked: toolbarHandler.zoomIn()
         }

         FlatButton {
            id: zoomOut
            icon: IconCode.ZOOM_OUT
            onClicked: toolbarHandler.zoomOut()
         }

         FlatButton {
            id: fitSelection
            icon: IconCode.ZOOM_FIT_SELECTION
            visible: root.workspaceMode === Workspace.Mode.Classic ||
                  root.workspaceMode === Workspace.Mode.SpectralEditing
            onClicked: toolbarHandler.fitSelection()
         }

         FlatButton {
            id: fitProject
            icon: IconCode.ZOOM_FIT_PROJECT
            visible: root.workspaceMode === Workspace.Mode.Classic ||
                     root.workspaceMode === Workspace.Mode.SpectralEditing
            onClicked: toolbarHandler.fitProject()
         }

         FlatButton {
            id: zoomToggle
            icon: IconCode.ZOOM_TOGGLE
            visible: root.workspaceMode === Workspace.Mode.Classic ||
                     root.workspaceMode === Workspace.Mode.SpectralEditing
            onClicked: toolbarHandler.zoomToggle()
         }
      }

      RowLayout {
         spacing: 1
         visible: root.workspaceMode !== Workspace.Mode.SimpleRecording

        FlatButton {
            id: trim
            icon: IconCode.TRIM
            onClicked: toolbarHandler.trim()
         }

         FlatButton {
            id: silence
            icon: IconCode.SILENCE
            onClicked: toolbarHandler.silence()
         }
      }

      ToolbarSeparator {
         visible: separatorVisible
      }
   }
}
