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

   //readonly property string theme: UiTheme.currentTheme
   property alias workspaceMode: toolsToolbar.workspaceMode
   property alias enableVolumeTester: toolsToolbar.enableVolumeTester
   property string language: "en"

   function disallowWindowResizing() {
      root.flags =  Qt.Window | Qt.WindowTitleHint | Qt.CustomizeWindowHint | Qt.WindowCloseButtonHint | Qt.WindowSystemMenuHint
      root.minimumWidth = root.width
      root.minimumHeight = root.height
      root.maximumWidth = root.width
      root.maximumHeight = root.height
   }

   function allowWindowResizing() {
      root.minimumWidth = window.minimumWidth
      root.minimumHeight = window.minimumHeight
      root.maximumWidth = window.maximumWidth
      root.maximumHeight = window.maximumHeight
      root.flags = Qt.Window
   }

   QtObject {
      id: window

      property int minimumWidth
      property int minimumHeight
      property int maximumWidth
      property int maximumHeight
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
            model: UiTheme.themes
            MenuItem {
               required property string modelData
               text: modelData
               //checkable: true
               //checked: theme === text
               onTriggered: {
                  UiTheme.applyTheme(text)
                  toolsToolbar.refreshSetup()
                  timelineRuler.updateTheme()
               }
            }

            onObjectAdded: (index, object) => themeMenu.insertItem(index, object)
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

            onObjectAdded: (index, object) => extras.insertItem(index, object)
            onObjectRemoved: extras.removeItem(object)
         }
      }
   }

   header: ToolsToolbar {
      id: toolsToolbar

      onSetupClicked: customiseToolbar.show()

      onPlaybackStarted: {
         trackCanvasView.contentItem.contentX = 0
         scrollableRuler.start()
         stalk.visible = true
      }

      onPlaybackStopped: {
         stalk.visible = false
         scrollableRuler.stop()
      }

      onPlaybackPaused: scrollableRuler.pause()

      onUpdateStatusBar: function(status) {
         statusBar.text = status
         timer.restart()
      }
   }

   ScrollView {
      id: trackCanvasView
      x: sidebar.width
      width: root.width - sidebar.width
      anchors.top: scrollableRuler.bottom
      anchors.bottom: footerId.top
      contentWidth: trackCanvasBackground.width

      ScrollBar.horizontal.onVisualPositionChanged: {
         scrollableRuler.scrollTo(trackCanvasView.contentItem.contentX)
      }

      Rectangle {
         id: trackCanvasBackground
         color: UiTheme.backgroundColor3

         width: {
            var contentWidth = trackCanvasBackground.childrenRect.x + trackCanvasBackground.childrenRect.width
            if (contentWidth > trackCanvasView.width) {
               return contentWidth
            } else {
               return trackCanvasView.width
            }
         }

         height: {
            var contentHeight = trackCanvasBackground.childrenRect.y + trackCanvasBackground.childrenRect.height
            if (contentHeight > trackCanvasView.height) {
               return contentHeight
            } else {
               return trackCanvasView.height
            }
         }

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

         WaveClipView {
            id: waveClipView2
            x: 13
            y: 86
            height: 124
            name: "Stereo 1"

            onOptionsClicked: {
               statusBar.text = waveClipView2.name + " options clicked"
               timer.restart()
            }
         }

         WaveClipView {
            id: waveClipView3
            x: 13
            y: 212
            height: 124
            name: "Mono 1"

            onOptionsClicked: {
               statusBar.text = waveClipView2.name + " options clicked"
               timer.restart()
            }
         }

         Rectangle {
            id: stalk
            x: 12
            width: 3
            anchors.top: trackCanvasBackground.top
            anchors.bottom: trackCanvasBackground.bottom
            color: UiTheme.fontColor2
            visible: false

            border {
               color: UiTheme.backgroundColor4
               width: 1
            }
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

   ScrollableRuler {
      id: scrollableRuler
      x: sidebar.width
      width: trackCanvasView.width
      height: 32
      contentWidth: trackCanvasBackground.width

      onContentChanged: (playheadCursorPosition, scrolledTo) => {
         stalk.x = playheadCursorPosition + 7
         trackCanvasBackground.width = scrollableRuler.contentWidth
         trackCanvasView.contentItem.contentX = scrolledTo
      }
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

   Component.onCompleted: {
      window.minimumWidth = root.minimumWidth
      window.minimumHeight = root.minimumHeight
      window.maximumWidth = root.maximumWidth
      window.maximumHeight = root.maximumHeight
   }
}
