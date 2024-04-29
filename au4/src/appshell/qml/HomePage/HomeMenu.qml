/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * Music Composition & Notation
 *
 * Copyright (C) 2024 Audacity BVBA and others
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
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Muse.Cloud 1.0

Item {
    id: root

    property string currentPageName: ""
    property bool iconsOnly: false

    signal selected(string name)

    NavigationSection {
        id: navSec
        name: "HomeMenuSection"
        enabled: root.enabled && root.visible
        order: 2
    }

    NavigationPanel {
        id: navPanel
        name: "HomeMenuPanel"
        enabled: root.enabled && root.visible
        section: navSec
        order: 1
        direction: NavigationPanel.Vertical

        accessible.name: qsTrc("appshell", "Home menu") + " " + navPanel.directionInfo
    }

    ColumnLayout {
        anchors.fill: parent

        spacing: 0

        AccountInfoButton {
            Layout.fillWidth: true
            Layout.preferredHeight: 60
            Layout.topMargin: 20

            navigation.name: "AccountInfo"
            navigation.panel: navPanel
            navigation.row: 1

            checked: root.currentPageName === "account"
            iconOnly: root.iconsOnly

            onToggled: {
                root.selected("account")
            }
        }

        RadioButtonGroup {
            id: radioButtonList

            Layout.fillHeight: true
            Layout.fillWidth: true

            orientation: ListView.Vertical
            spacing: 0

            model: [
                { "name": "projects", "title": qsTrc("appshell", "Project"), "icon": IconCode.WAVEFORM },
                { "name": "learn", "title": qsTrc("appshell", "Learn"), "icon":  IconCode.LEARN }
            ]

            currentIndex: 0

            delegate: PageTabButton {
                id: radioButtonDelegate

                width: parent.width

                navigation.name: title
                navigation.panel: navPanel
                navigation.row: 2 + model.index

                spacing: 30
                leftPadding: spacing

                ButtonGroup.group: radioButtonList.radioButtonGroup
                orientation: Qt.Horizontal
                checked: modelData["name"] === root.currentPageName

                title: modelData["title"]
                iconOnly: root.iconsOnly

                iconComponent: StyledIconLabel {
                    iconCode: modelData["icon"]
                }

                onToggled: {
                    radioButtonList.currentIndex = index
                    root.selected(modelData["name"])
                }
            }
        }
    }
}
