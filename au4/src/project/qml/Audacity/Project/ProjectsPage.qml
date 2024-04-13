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
import QtQuick.Layouts 1.3

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Audacity.Project 1.0
import Muse.Cloud 1.0

import "internal/ProjectsPage"

FocusScope {
    id: root

    QtObject {
        id: prv

        readonly property int sideMargin: 46
    }

    NavigationSection {
        id: navSec
        name: "RecentProjects"
        enabled: root.enabled && root.visible
        order: 3
        onActiveChanged: {
            if (navSec.active) {
                root.forceActiveFocus()
            }
        }
    }

    ProjectsPageModel {
        id: projectsPageModel
    }

    Component.onCompleted: {
        tabBar.currentIndex = projectsPageModel.tabIndex
        tabBar.completed = true
    }

    Rectangle {
        id: background

        anchors.fill: parent

        color: ui.theme.backgroundSecondaryColor
    }

    RowLayout {
        id: topLayout

        anchors.top: parent.top
        anchors.topMargin: prv.sideMargin
        anchors.left: parent.left
        anchors.leftMargin: prv.sideMargin
        anchors.right: parent.right
        anchors.rightMargin: prv.sideMargin

        spacing: 12

        NavigationPanel {
            id: navSearchPanel
            name: "HomeProjectsSearch"
            enabled: topLayout.enabled && topLayout.visible
            section: navSec
            order: 1
            accessible.name: qsTrc("project", "Projects")
        }

        StyledTextLabel {
            id: pageTitle
            Layout.fillWidth: true

            text: qsTrc("project", "Projects")
            font: ui.theme.titleBoldFont
            horizontalAlignment: Text.AlignLeft
        }

        SearchField {
            id: searchField

            Layout.preferredWidth: 220

            navigation.name: "Projects Search"
            navigation.panel: navSearchPanel
            navigation.order: 1
            accessible.name: qsTrc("project", "Search recent projects")
        }
    }

    RowLayout {
        id: controlsRow

        anchors.top: topLayout.bottom
        anchors.topMargin: prv.sideMargin
        anchors.left: parent.left
        anchors.leftMargin: prv.sideMargin
        anchors.right: parent.right
        anchors.rightMargin: prv.sideMargin

        spacing: 12

        StyledTabBar {
            id: tabBar

            property bool completed: false

            Layout.fillWidth: true

            onCurrentIndexChanged: {
                if (completed) {
                    projectsPageModel.tabIndex = currentIndex
                }
            }

            NavigationPanel {
                id: navTabPanel
                name: "HomeProjectsTabs"
                section: navSec
                direction: NavigationPanel.Horizontal
                order: 2
                accessible.name: qsTrc("project", "Projects tab bar")
                enabled: tabBar.enabled && tabBar.visible

                onNavigationEvent: function(event) {
                    if (event.type === NavigationEvent.AboutActive) {
                        event.setData("controlName", tabBar.currentItem.navigation.name)
                    }
                }
            }

            StyledTabButton {
                text: qsTrc("project", "New & recent")

                navigation.name: "NewAndRecent"
                navigation.panel: navTabPanel
                navigation.column: 1
            }

            StyledTabButton {
                text: qsTrc("project", "My online projects")

                navigation.name: "MyOnlineProjects"
                navigation.panel: navTabPanel
                navigation.column: 2
            }
        }

        NavigationPanel {
            id: viewButtonsNavPanel
            name: "ViewButtons"
            enabled: tabBar.enabled && tabBar.visible
            section: navSec
            order: 3
            direction: NavigationPanel.Horizontal
            accessible.name: qsTrc("project", "View buttons")
        }

        FlatButton {
            id: refreshButton

            visible: tabBar.currentIndex === 1

            navigation.panel: viewButtonsNavPanel
            navigation.order: 1

            icon: IconCode.UPDATE
            text: qsTrc("project", "Refresh")
            orientation: Qt.Horizontal
        }

        RadioButtonGroup {
            id: viewTypeRadioButtons

            property int navigationOrderStart: refreshButton.navigation.order + 1

            implicitHeight: ui.theme.defaultButtonSize

            model: [
                { "icon": IconCode.GRID, "title": qsTrc("project", "Grid view"), "value": ProjectsPageModel.Grid },
                { "icon": IconCode.LIST, "title": qsTrc("project", "List view"), "value": ProjectsPageModel.List }
            ]

            delegate: FlatRadioButton {
                implicitWidth: ui.theme.defaultButtonSize
                implicitHeight: ui.theme.defaultButtonSize

                checked: projectsPageModel.viewType === modelData.value

                iconCode: modelData.icon
                transparent: true
                checkedColor: ui.theme.buttonColor

                navigation.name: "ViewType_" + modelData.title
                navigation.panel: viewButtonsNavPanel
                navigation.order: viewTypeRadioButtons.navigationOrderStart + model.index
                navigation.accessible.name: modelData.title

                onToggled: {
                    projectsPageModel.viewType = modelData.value
                }
            }
        }
    }

    Loader {
        id: contentLoader

        anchors.top: controlsRow.bottom
        anchors.topMargin: 24
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: buttonsPanel.top

        sourceComponent: {
            if (!tabBar.completed || tabBar.currentIndex < 0) {
                return null
            }

            return [newAndRecentComp, onlineProjectsComp][tabBar.currentIndex]
        }
    }

    Component {
        id: newAndRecentComp

        RecentProjectsView {
            anchors.fill: parent

            viewType: projectsPageModel.viewType
            searchText: searchField.searchText

            backgroundColor: background.color
            sideMargin: prv.sideMargin

            navigationSection: navSec
            navigationOrder: 4

            onCreateNewProjectRequested: {
                projectsPageModel.createNewProject()
            }

            onOpenProjectRequested: function(projectPath, displayName) {
                Qt.callLater(projectsPageModel.openProject, projectPath, displayName)
            }
        }
    }

    Component {
        id: onlineProjectsComp

        Text {
            text: "onlineProjectsComp"
        }

        // CloudProjectsView {
        //     id: cloudProjectsView
        //     anchors.fill: parent

        //     viewType: projectsPageModel.viewType
        //     searchText: searchField.searchText

        //     backgroundColor: background.color
        //     sideMargin: prv.sideMargin

        //     navigationSection: navSec
        //     navigationOrder: 4

        //     onCreateNewProjectRequested: {
        //         projectsPageModel.createNewProject()
        //     }

        //     onOpenProjectRequested: function(projectPath, displayName) {
        //         Qt.callLater(projectsPageModel.openProject, projectPath, displayName)
        //     }

        //     Connections {
        //         target: refreshButton

        //         function onClicked() {
        //             cloudProjectsView.refresh()
        //         }
        //     }
        // }
    }

    Rectangle {
        id: buttonsPanel

        anchors.bottom: parent.bottom

        height: 100
        width: parent.width

        color: ui.theme.backgroundSecondaryColor

        NavigationPanel {
            id: navBottomPanel
            name: "RecentProjectsBottom"
            section: navSec
            direction: NavigationPanel.Horizontal
            order: 5

            //: accessibility name for the panel at the bottom of the "Projects" page
            accessible.name: qsTrc("project", "Projects actions")
        }

        FlatButton {
            anchors.left: parent.left
            anchors.leftMargin: prv.sideMargin
            anchors.verticalCenter: parent.verticalCenter

            navigation.name: "ProjectManager"
            navigation.panel: navBottomPanel
            navigation.column: 1

            minWidth: 216
            text: qsTrc("project", "Project manager (online)")

            onClicked: {
                projectsPageModel.openProjectManager()
            }
        }

        Row {
            anchors.right : parent.right
            anchors.rightMargin: prv.sideMargin
            anchors.verticalCenter: parent.verticalCenter

            spacing: 22

            FlatButton {
                navigation.name: "NewProject"
                navigation.panel: navBottomPanel
                navigation.column: 2

                text: qsTrc("project", "New")

                onClicked: {
                    projectsPageModel.createNewProject()
                }
            }

            FlatButton {
                navigation.name: "Open other Project"
                navigation.panel: navBottomPanel
                navigation.column: 3

                text: qsTrc("project", "Open other…")

                onClicked: {
                    projectsPageModel.openOther()
                }
            }
        }
    }
}
