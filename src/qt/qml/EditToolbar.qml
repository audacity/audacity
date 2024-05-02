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

   objectName: "EditToolbar"

   property bool gripVisible: false
   property bool separatorVisible: true

   property var workspaceMode: Workspace.Mode.Classic

   signal updateStatusBar(status: string)

   function registerToolbarConfiguration() {
      toolbarHandler.RegisterToolbarConfiguration()
   }

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
            color: ui.theme.backgroundColor1
            visible: toolbarHandler.automationVisible
         }

         FlatButton {
            id: automation
            icon: IconCode.AUTOMATION
            visible: toolbarHandler.automationVisible &&
                     (root.workspaceMode === Workspace.Mode.Classic ||
                     root.workspaceMode === Workspace.Mode.AudioEditing)
            onClicked: toolbarHandler.Automation()
         }
      }

      RowLayout {
         spacing: 1

         FlatButton {
            id: zoomIn
            icon: IconCode.ZOOM_IN
            visible: toolbarHandler.zoomInVisible
            onClicked: toolbarHandler.ZoomIn()
         }

         FlatButton {
            id: zoomOut
            icon: IconCode.ZOOM_OUT
            visible: toolbarHandler.zoomOutVisible
            onClicked: toolbarHandler.ZoomOut()
         }

         FlatButton {
            id: fitSelection
            icon: IconCode.ZOOM_FIT_SELECTION
            visible: toolbarHandler.fitSelectionVisible &&
                     (root.workspaceMode === Workspace.Mode.Classic ||
                     root.workspaceMode === Workspace.Mode.SpectralEditing)
            onClicked: toolbarHandler.FitSelection()
         }

         FlatButton {
            id: fitProject
            icon: IconCode.ZOOM_FIT_PROJECT
            visible: toolbarHandler.fitProjectVisible &&
                     (root.workspaceMode === Workspace.Mode.Classic ||
                     root.workspaceMode === Workspace.Mode.SpectralEditing)
            onClicked: toolbarHandler.FitProject()
         }

         FlatButton {
            id: zoomToggle
            icon: IconCode.ZOOM_TOGGLE
            visible: toolbarHandler.zoomToggleVisible &&
                     (root.workspaceMode === Workspace.Mode.Classic ||
                     root.workspaceMode === Workspace.Mode.SpectralEditing)
            onClicked: toolbarHandler.ZoomToggle()
         }
      }

      RowLayout {
         spacing: 1
         visible: (toolbarHandler.trimVisible || toolbarHandler.silenceVisible) &&
                  (root.workspaceMode !== Workspace.Mode.SimpleRecording)

        FlatButton {
            id: trim
            icon: IconCode.TRIM
            visible: toolbarHandler.trimVisible
            onClicked: toolbarHandler.Trim()
         }

         FlatButton {
            id: silence
            icon: IconCode.SILENCE
            visible: toolbarHandler.silenceVisible
            onClicked: toolbarHandler.Silence()
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
