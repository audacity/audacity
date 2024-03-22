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

import MuseScore.Ui 1.0

import "internal"

ListView {
    id: root

    property bool arrowControlsAvailable: false
    property int scrollBarThickness: 8
    property int scrollBarPolicy: ScrollBar.AsNeeded

    /// Includes the margin at the border side; excludes the margin at the content side
    property int visualScrollBarInset: 1.5 * scrollBarThickness

    clip: true
    boundsBehavior: Flickable.StopAtBounds
    maximumFlickVelocity: ui.theme.flickableMaxVelocity

    ScrollBar.vertical: root.arrowControlsAvailable ? null : scrollBarComp.createObject(root)
    ScrollBar.horizontal: root.arrowControlsAvailable ? null : scrollBarComp.createObject(root)

    property alias navigation: navPanel
    property alias accessible: navPanel.accessible

    NavigationPanel {
        id: navPanel
        name: root.objectName !== "" ? root.objectName : "ListView"
        enabled: root.enabled && root.visible

        accessible.role: MUAccessible.List
        accessible.name: "List"
        accessible.visualItem: root
        accessible.enabled: navPanel.enabled

        onNavigationEvent: function(event) {
            if (event.type === NavigationEvent.AboutActive) {
                for (var i = 0; i < root.count; ++i) {
                    var item = root.itemAtIndex(i)
                    if (item.isSelected) {
                        event.setData("controlIndex", [item.navigation.row, item.navigation.column])
                        return
                    }
                }

                var firstItem = root.itemAtIndex(0)
                if (Boolean(firstItem)) {
                    event.setData("controlIndex", [firstItem.navigation.row, firstItem.navigation.column])
                }
            }
        }
    }

    Component {
        id: scrollBarComp

        StyledScrollBar {
            thickness: root.scrollBarThickness
            policy: root.scrollBarPolicy
        }
    }

    ArrowScrollButton {
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right

        icon: IconCode.UP
        isScrollUp: true
        view: root

        enabled: root.arrowControlsAvailable
    }

    ArrowScrollButton {
        anchors.bottom: parent.bottom
        anchors.left: parent.left
        anchors.right: parent.right

        icon: IconCode.DOWN
        isScrollUp: false
        view: root

        enabled: root.arrowControlsAvailable
    }
}
