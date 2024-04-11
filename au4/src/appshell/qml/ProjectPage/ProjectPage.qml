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
import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Muse.Dock 1.0
import Audacity.AppShell 1.0
import Audacity.ProjectScene 1.0

import "../dockwindow"

DockPage {
    id: root

    objectName: "ProjecPage"
    uri: "musescore://project"

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

    readonly property int verticalPanelDefaultWidth: 300

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

            ProjectToolBar {
                navigationPanel.section: root.topToolKeyNavSec
                navigationPanel.order: 2

                onActiveFocusRequested: {
                    if (navigationPanel.active) {
                        projectToolBar.forceActiveFocus()
                    }
                }
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
                root.toolBarBottomDropDestination,
                root.toolBarLeftDropDestination,
                root.toolBarRightDropDestination
            ]

            PlaybackToolBar {
                orientation: playbackToolBar.orientation
                floating: playbackToolBar.floating

                maximumWidth: playbackToolBar.width
                maximumHeight: playbackToolBar.height

                navigationPanel.section: root.playbackToolBarKeyNavSec
                navigationPanel.order: 1
            }
        }
    ]

    panels: [
        DockPanel {
            id: tracksPanel

            objectName: pageModel.tracksPanelName()
            title: qsTrc("appshell", "Tracks")

            navigationSection: root.navigationPanelSec(tracksPanel.location)

            width: root.verticalPanelDefaultWidth
            minimumWidth: root.verticalPanelDefaultWidth
            maximumWidth: root.verticalPanelDefaultWidth

            groupName: root.verticalPanelsGroup

            dropDestinations: root.verticalPanelDropDestinations

            TracksPanel {
                navigationSection: tracksPanel.navigationSection

                Component.onCompleted: {
                    tracksPanel.contextMenuModel = contextMenuModel
                }
            }
        }
    ]

    central: ClipsView {
        id: clipsView
    }
}
