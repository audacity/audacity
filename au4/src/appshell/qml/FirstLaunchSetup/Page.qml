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

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Audacity.AppShell 1.0

Item {
    id: root

    property alias title: titleLabel.text
    property alias explanation: explanationLabel.text

    property NavigationSection navigationSection: null
    property int navigationStartRow: 2
    property string activeButtonTitle: ""

    default property alias content: contentItem.data

    property real titleContentSpacing: 24

    property string extraButtonTitle: ""
    signal extraButtonClicked()

    anchors.fill: parent

    function readInfo() {
        accessibleInfo.readInfo()
    }

    function resetFocus() {
        accessibleInfo.resetFocus()
    }

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "ContentPanel"
        enabled: root.enabled && root.visible
        section: root.navigationSection
        order: root.navigationStartRow
        direction: NavigationPanel.Vertical
    }

    AccessibleItem {
        id: accessibleInfo

        accessibleParent: root.navigationPanel.accessible
        visualItem: root
        role: MUAccessible.Button
        name: root.title + ". " + root.explanation + " " + root.activeButtonTitle

        function readInfo() {
            accessibleInfo.ignored = false
            accessibleInfo.focused = true
        }

        function resetFocus() {
            accessibleInfo.ignored = true
            accessibleInfo.focused = false
        }
    }

    Column {
        id: header

        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right

        height: childrenRect.height
        spacing: 6

        StyledTextLabel {
            id: titleLabel

            anchors.horizontalCenter: parent.horizontalCenter
            width: parent.width

            font: ui.theme.largeBodyBoldFont
            wrapMode: Text.Wrap
        }

        StyledTextLabel {
            id: explanationLabel

            anchors.horizontalCenter: parent.horizontalCenter
            width: parent.width

            wrapMode: Text.Wrap
        }
    }

    Item {
        id: contentItem

        anchors.top: header.bottom
        anchors.topMargin: root.titleContentSpacing
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
    }
}
