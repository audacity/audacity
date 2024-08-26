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

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Muse.Dock 1.0

import Audacity.Project 1.0
import Muse.Cloud 1.0
import Muse.Learn 1.0

DockPage {
    id: root

    property string section: "projects"
    property string subSection: ""

    property var window: null

    objectName: "Home"
    uri: "musescore://home"

    onSetParamsRequested: function(params) {
        if (Boolean(params["section"])) {
            setCurrentCentral(params["section"])

            if (Boolean(params["subSection"])) {
                subSection = params["subSection"]
            }
        }
    }

    onSectionChanged: {
        Qt.callLater(root.setCurrentCentral, section)
    }

    function setCurrentCentral(name) {
        if (section === name || !Boolean(name)) {
            return
        }

        section = name

        switch (name) {
        case "projects": root.central = projetsComp; break
        case "learn": root.central = learnComp; break
        case "account": root.central = accountComp; break
        }
    }

    panels: [
        DockPanel {
            id: menuPanel

            objectName: "homeMenu"

            readonly property int maxFixedWidth: 260
            readonly property int minFixedWidth: 76
            readonly property bool iconsOnly: root.window
                                                ? root.window.width < (root.window.minimumWidth + maxFixedWidth - minFixedWidth)
                                                : false
            readonly property int currentFixedWidth: iconsOnly ? minFixedWidth : maxFixedWidth

            width: currentFixedWidth
            minimumWidth: currentFixedWidth
            maximumWidth: currentFixedWidth

            floatable: false
            closable: false

            HomeMenu {
                currentPageName: root.section
                iconsOnly: menuPanel.iconsOnly

                onSelected: function(name) {
                    root.setCurrentCentral(name)
                }
            }
        }
    ]

    central: projetsComp

    Component {
        id: accountComp
        AccountPage {}
    }

    Component {
        id: projetsComp

        ProjectsPage {}
    }

    Component {
        id: learnComp

        LearnPage {
            section: root.subSection
        }
    }
}
