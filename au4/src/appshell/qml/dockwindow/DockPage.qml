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

import MuseScore.Dock 1.0

DockPageView {
    id: root

    property Component central: null

    property var toolBarLeftDropDestination: { "dock": toolBarLeftDockingHolder, "dropLocation": Location.Top }
    property var toolBarRightDropDestination: { "dock": toolBarRightDockingHolder, "dropLocation": Location.Top }
    property var toolBarTopDropDestination: { "dock": toolBarTopDockingHolder, "dropLocation": Location.Left }
    property var toolBarBottomDropDestination: { "dock": toolBarBottomDockingHolder, "dropLocation": Location.Left }

    property var panelLeftDropDestination: { "dock": panelLeftDockingHolder, "dropLocation": Location.Top }
    property var panelRightDropDestination: { "dock": panelRightDockingHolder, "dropLocation": Location.Top }
    property var panelTopDropDestination: { "dock": panelTopDockingHolder, "dropLocation": Location.Left }
    property var panelBottomDropDestination: { "dock": panelBottomDockingHolder, "dropLocation": Location.Left }

    property bool completed: false
    Component.onCompleted: root.completed = true
    Component.onDestruction: root.completed = false

    centralDock: DockCentralView {
        objectName: root.objectName + "_central"

        contentNavigationPanel: Boolean(central.item) && Boolean(central.item.navigationPanel) ?
                                    central.item.navigationPanel : null

        Loader {
            id: central
            anchors.fill: parent
            sourceComponent: (root.completed && root.visible) ? root.central : null
        }
    }

    toolBarsDockingHolders: [
        DockingHolder {
            id: toolBarLeftDockingHolder

            objectName: root.objectName + "_toolBarsDockingHolderLeft"
            location: Location.Left
        },
        DockingHolder {
            id: toolBarRightDockingHolder

            objectName: root.objectName + "_toolBarsDockingHolderRight"
            location: Location.Right
        },
        DockingHolder {
            id: toolBarTopDockingHolder

            objectName: root.objectName + "_toolBarsDockingHolderTop"
            location: Location.Top
        },
        DockingHolder {
            id: toolBarBottomDockingHolder

            objectName: root.objectName + "_toolBarsDockingHolderBottom"
            location: Location.Bottom
        }
    ]

    panelsDockingHolders: [
        DockingHolder {
            id: panelLeftDockingHolder

            objectName: root.objectName + "_panelsDockingHolderLeft"
            location: Location.Left
        },
        DockingHolder {
            id: panelRightDockingHolder

            objectName: root.objectName + "_panelsDockingHolderRight"
            location: Location.Right
        },
        DockingHolder {
            id: panelTopDockingHolder

            objectName: root.objectName + "_panelsDockingHolderTop"
            location: Location.Top
        },
        DockingHolder {
            id: panelBottomDockingHolder

            objectName: root.objectName + "_panelsDockingHolderBottom"
            location: Location.Bottom
        }
    ]
}
