import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Qt.labs.platform

import Audacity
import Audacity.UiComponents
import Audacity.UiThemes

ApplicationWindow {
   id: root
   width: 1368
   height: 912
   visible: true
   title: qsTr("Audacity")
   minimumWidth: 480
   minimumHeight: 540

   readonly property string theme: UiTheme.currentTheme
   property alias workspaceMode: toolsToolbar.workspaceMode
   property alias enableVolumeTester: toolsToolbar.enableVolumeTester
   property string language: "en"

   QtObject {
      id: testers
      property bool volumeVisible: false
   }

   TranslationManager {
      id: translationManager
   }

   CustomiseToolbar {
      id: customiseToolbar
      visible: false
   }

   MenuBar {
      Menu {
         title: qsTr("File")
         MenuItem {
            text: qsTr("Exit")
            onTriggered: close()
         }
      }

      Menu {
         title: qsTr("Workspace")
         MenuItem {
            text: qsTr("Classic")
            checkable: true
            checked: root.workspaceMode === Workspace.Mode.Classic
            onTriggered: {
               workspaceMode = Workspace.Mode.Classic
            }
         }

         MenuItem {
            text: qsTr("Simple recording")
            checkable: true
            checked: root.workspaceMode === Workspace.Mode.SimpleRecording
            onTriggered: {
               workspaceMode = Workspace.Mode.SimpleRecording
            }
         }

         MenuItem {
            text: qsTr("Audio editing")
            checkable: true
            checked: root.workspaceMode === Workspace.Mode.AudioEditing
            onTriggered: {
               workspaceMode = Workspace.Mode.AudioEditing
            }
         }

         MenuItem {
            text: qsTr("Spectral editing")
            checkable: true
            checked: root.workspaceMode === Workspace.Mode.SpectralEditing
            onTriggered: {
               workspaceMode = Workspace.Mode.SpectralEditing
            }
         }
      }

      Menu {
         title: qsTr("Language")
         MenuItem {
            text: "English"
            checkable: true
            checked: language === "en"
            onTriggered:{
               language = "en"
               translationManager.ChangeLanguage(language)
               toolsToolbar.refreshSetup()
            }
         }

         MenuItem {
            text: "Deutsch"
            checkable: true
            checked: language === "de"
            onTriggered: {
               language = "de"
               translationManager.ChangeLanguage(language)
               toolsToolbar.refreshSetup()
            }
         }
      }

      Menu {
         id: themeMenu
         title: qsTr("Theme")

         Instantiator {
            model: UiTheme.availableThemes()
            MenuItem {
               required property string modelData
               text: modelData
               checkable: true
               checked: theme === text
               onTriggered: {
                  UiTheme.changeTheme(text)
                  toolsToolbar.refreshSetup()
                  timelineRuler.updateTheme()
               }
            }

            onObjectAdded: themeMenu.insertItem(index, object)
            onObjectRemoved: themeMenu.removeItem(object)
         }
      }

      Menu {
         title: qsTr("Testers")
         MenuItem {
            text: qsTr("VolumeControl")
            checkable: true
            checked: enableVolumeTester
            onTriggered: {
               enableVolumeTester = !enableVolumeTester
            }
         }
      }

      Menu {
         id: extras
         title : qsTr("Extra")

         Instantiator {
            model : extraMenu.items
            MenuItem {
               text : modelData
               onTriggered : extraMenu.activate(index)
            }

            onObjectAdded: extras.insertItem(index, object)
            onObjectRemoved: extras.removeItem(object)
         }
      }
   }

   header: ToolsToolbar {
      id: toolsToolbar

      onSetupClicked: customiseToolbar.show()
      onPlaybackStarted: timelineRuler.start()
      onPlaybackStopped: timelineRuler.stop()
      onPlaybackPaused: timelineRuler.pause()

      onUpdateStatusBar: function(status) {
         statusBar.text = status
         timer.restart()
      }
   }

   Rectangle {
      id: trackCanvas
      x: sidebar.width
      width: root.width - sidebar.width
      color: UiTheme.backgroundColor3
      anchors.top: timelineRuler.bottom
      anchors.bottom: footerId.top

      WaveClipView {
         id: waveClipView
         x: 13
         y: 0
         height: 84
         name: "Audio 1"

         onOptionsClicked: {
            statusBar.text = waveClipView.name + " options clicked"
            timer.restart()
         }
      }
   }

   Sidebar {
      id: sidebar
      anchors.top: toolsToolbar.bottom
      anchors.bottom: footerId.top
      onUpdateStatusBar: function(status) {
         statusBar.text = status
         timer.restart()
      }
   }

   Timer {
      id: timer
      interval: 1000
      repeat: false
      onTriggered: statusBar.text = ""
   }

   TimelineRuler {
      id: timelineRuler
      x: sidebar.width + 1
      width: root.width - sidebar.width - 1
      playheadCursorHeight: height / 2 + trackCanvas.height
   }

   footer: Rectangle {
      id: footerId
      width: parent.width
      height: 30
      color: UiTheme.backgroundColor1

      Text {
         id: statusBar
         color: UiTheme.fontColor1
         anchors.centerIn: parent
      }

      Rectangle {
         anchors.bottom: parent.top
         height: 1
         width: parent.width
         color: UiTheme.strokeColor2
      }
   }
}
