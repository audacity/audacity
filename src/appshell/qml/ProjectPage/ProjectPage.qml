/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
import QtQuick
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents
import Muse.Dock

import Audacity.AppShell
import Audacity.ProjectScene
import Audacity.Playback


DockPage {
    id: root

    objectName: "ProjecPage"
    uri: "audacity://project"

    property var topToolKeyNavSec

    property ProjectPageModel pageModel: ProjectPageModel {}

    property NavigationSection playbackToolBarKeyNavSec: NavigationSection {
        id: keynavSec
        name: "PlaybackSection"
        order: 2
    }

    property NavigationSection keynavTopPanelSec: NavigationSection {
        name: "NavigationTopPanel"
        enabled: root.visible
        order: 3
    }

    property NavigationSection keynavLeftPanelSec: NavigationSection {
        name: "NavigationLeftPanel"
        enabled: root.visible
        order: 4
    }

    property NavigationSection keynavRightPanelSec: NavigationSection {
        name: "NavigationRightPanel"
        enabled: root.visible
        order: 6
    }

    property NavigationSection keynavBottomPanelSec: NavigationSection {
        name: "NavigationBottomPanel"
        enabled: root.visible
        order: 7
    }

    function navigationPanelSec(location) {
        switch(location) {
        case Location.Top: return keynavTopPanelSec
        case Location.Left: return keynavLeftPanelSec
        case Location.Right: return keynavRightPanelSec
        case Location.Bottom: return keynavBottomPanelSec
        }

        return null
    }

    onInited: {
        Qt.callLater(pageModel.init)
    }

    readonly property int verticalPanelDefaultWidth: 281

    readonly property int horizontalPanelMinHeight: 100
    readonly property int horizontalPanelMaxHeight: 520

    readonly property string verticalPanelsGroup: "VERTICAL_PANELS"
    readonly property string horizontalPanelsGroup: "HORIZONTAL_PANELS"

    readonly property var verticalPanelDropDestinations: [
        { "dock": root.centralDock, "dropLocation": Location.Left, "dropDistance": root.verticalPanelDefaultWidth },
        { "dock": root.centralDock, "dropLocation": Location.Right, "dropDistance": root.verticalPanelDefaultWidth }
    ]

    readonly property var horizontalPanelDropDestinations: [
        root.panelTopDropDestination,
        root.panelBottomDropDestination
    ]

    mainToolBars: [
        DockToolBar {
            id: projectToolBar

            objectName: pageModel.projectToolBarName()
            title: qsTrc("appshell", "Project toolbar")

            floatable: false
            closable: false
            resizable: false
            separatorsVisible: false

            alignment: DockToolBarAlignment.Center
            contentBottomPadding: 2

            compactPriorityOrder: 1

            ProjectToolBar {
                isCompactMode: projectToolBar.isCompact

                navigationPanel.section: root.topToolKeyNavSec
                navigationPanel.order: 2
            }
        },

        DockToolBar {
            id: undoRedoToolBar

            objectName: pageModel.undoRedoToolBarName()
            title: qsTrc("appshell", "Undo/redo toolbar")

            floatable: false
            closable: false
            resizable: false
            separatorsVisible: false

            alignment: DockToolBarAlignment.Right
            contentBottomPadding: 2

            UndoRedoToolBar {
                navigationPanel.section: root.topToolKeyNavSec
                navigationPanel.order: 3
            }
        }
    ]

    toolBars: [
        DockToolBar {
            id: playbackToolBar

            objectName: pageModel.playbackToolBarName()
            title: qsTrc("appshell", "Play Tool Bar")

            dropDestinations: [
                root.toolBarTopDropDestination,
                root.toolBarBottomDropDestination
            ]

            //! small hack: needed so that when the screen is quickly reduced the dock system works correctly
            minimumWidth: 300

            PlaybackToolBar {
                floating: playbackToolBar.floating

                maximumWidth: playbackToolBar.width - 30 /* grip button */
                maximumHeight: playbackToolBar.height

                onHeightChanged: {
                    playbackToolBar.thickness = height
                }

                navigationPanel.section: root.playbackToolBarKeyNavSec
                navigationPanel.order: 1
            }
        }
    ]

    panels: [
        DockPanel {
            id: tracksPanel
            readonly property int effectsSectionWidth: 240
            property bool showEffectsSection: false
            property int titleBarHeight: 39

            // For some reason, I cannot refer to `tp` from anywhere other than `tp` itself
            // without getting a "tp is not defined" error. Maybe because this `DockPanel`
            // is within that array thingy ?
            signal effectsSectionVisibilityChanged(show: bool)

            onShowEffectsSectionChanged: {
                const newWidth = root.verticalPanelDefaultWidth + (tracksPanel.showEffectsSection ? tracksPanel.effectsSectionWidth : 0)
                tracksPanel.width = newWidth
                tracksPanel.minimumWidth = newWidth
                tracksPanel.maximumWidth = newWidth
                effectsSectionVisibilityChanged(showEffectsSection)
            }

            signal add(type: int)

            objectName: pageModel.tracksPanelName()
            title: qsTrc("appshell", "Tracks")

            navigationSection: root.navigationPanelSec(tracksPanel.location)

            width: root.verticalPanelDefaultWidth + (showEffectsSection ? effectsSectionWidth : 0)
            minimumWidth: width
            maximumWidth: width

            groupName: root.verticalPanelsGroup

            dropDestinations: root.verticalPanelDropDestinations

            titleBar: TracksTitleBar {

                id: titleBarItem
                effectsSectionWidth: tracksPanel.effectsSectionWidth
                showEffectsSection: tracksPanel.showEffectsSection
                implicitHeight: tracksPanel.titleBarHeight

                onAddRequested: function(type) {
                    tracksPanel.add(type)
                }

                onEffectsSectionCloseButtonClicked: {
                    tracksPanel.showEffectsSection = false
                }
            }

            TracksPanel {
                id: tp
                navigationSection: tracksPanel.navigationSection
                effectsSectionWidth: tracksPanel.effectsSectionWidth

                Component.onCompleted: {
                    tracksPanel.effectsSectionVisibilityChanged.connect(function(show) {
                        tp.showEffectsSection = show
                    })
                }

                onShowEffectsSectionChanged: {
                    tracksPanel.showEffectsSection = showEffectsSection
                }

                onOpenEffectsRequested: {
                    tracksPanel.showEffectsSection = true
                }

                Connections {
                    target: tracksPanel

                    function onAdd(type) {
                        tp.tracksModel.addTrack(type)
                    }
                }
            }
        }
    ]

    central: TracksClipsView {
        id: clipsView
    }

    statusBar: DockStatusBar {
        objectName: pageModel.statusBarName()

        height: 40
        minimumHeight: height
        maximumHeight: height

        ProjectStatusBar {}
    }
}
