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

import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0
import MuseScore.AppShell 1.0
import MuseScore.Dock 1.0

import MuseScore.NotationScene 1.0
import MuseScore.Playback 1.0

import "../dockwindow"
import "../NotationPage"

DockPage {
    id: root

    objectName: "Publish"
    uri: "musescore://publish"

    property var topToolKeyNavSec

    property NavigationSection publishToolBarKeyNavSec: NavigationSection {
        id: keynavSec
        name: "PublishToolBarSection"
        order: 2
    }

    property NotationPageModel pageModel: NotationPageModel {}

    mainToolBars: [
        DockToolBar {
            id: notationToolBar

            objectName: root.objectName + "_notationToolBar"
            title: qsTrc("appshell", "Notation toolbar")

            floatable: false
            closable: false
            resizable: false
            separatorsVisible: false

            alignment: DockToolBarAlignment.Center
            contentBottomPadding: 2

            NotationToolBar {
                navigationPanel.section: root.topToolKeyNavSec
                navigationPanel.order: 2

                onActiveFocusRequested: {
                    if (navigationPanel.active) {
                        notationToolBar.forceActiveFocus()
                    }
                }
            }
        },

        DockToolBar {
            id: playbackToolBar

            objectName: root.objectName + "_playbackToolBar"
            title: qsTrc("appshell", "Playback controls")

            separatorsVisible: false
            alignment: DockToolBarAlignment.Right
            contentBottomPadding: 2

            PlaybackToolBar {
                navigationPanelSection: root.topToolKeyNavSec
                navigationPanelOrder: 3

                floating: playbackToolBar.floating
            }
        },

        DockToolBar {
            id: undoRedoToolBar

            objectName: root.objectName + "_undoRedoToolBar"
            title: qsTrc("appshell", "Undo/redo toolbar")

            floatable: false
            closable: false
            resizable: false
            separatorsVisible: false

            alignment: DockToolBarAlignment.Right
            contentBottomPadding: 2

            UndoRedoToolBar {
                navigationPanel.section: root.topToolKeyNavSec
                navigationPanel.order: 4
            }
        }
    ]

    toolBars: [
        DockToolBar {
            objectName: "publishToolBar"

            floatable: false

            PublishToolBar {
                navigation.section: root.publishToolBarKeyNavSec
                navigation.order: 1
            }
        }
    ]

    central: NotationView {
        name: "PublishNotationView"
        publishMode: true
    }

    statusBar: DockStatusBar {
        objectName: "publishStatusBar"

        NotationStatusBar {}
    }
}
