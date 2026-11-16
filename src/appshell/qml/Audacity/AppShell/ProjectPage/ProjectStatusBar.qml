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
import QtQuick.Layouts

import Muse.UiComponents

import Audacity.ProjectScene

Item {
    id: root

    property NavigationSection navigationSection: NavigationSection {
        id: navSec
        name: "ProjectStatusBar"
        enabled: root.enabled && root.visible
        order: 1000
    }

    NavigationPanel {
        id: navPanel
        name: "ProjectStatusBar"
        enabled: root.enabled && root.visible
        accessible.name: qsTrc("appshell", "Selection status")
        order: 0
        direction: NavigationPanel.Horizontal
        section: navSec
    }

    RowLayout {
        id: statusBarRow

        readonly property int leftMargin: 12
        readonly property int rightMargin: 6

        anchors.left: parent.left
        anchors.leftMargin: statusBarRow.leftMargin
        anchors.right: parent.right
        anchors.rightMargin: statusBarRow.rightMargin

        height: parent.height

        SelectionStatus {
            Layout.alignment: Qt.AlignRight | Qt.AlignVCenter
            Layout.preferredWidth: width

            maximumWidth: root.width - statusBarRow.leftMargin - statusBarRow.rightMargin

            navigationPanel: navPanel
        }
    }
}
