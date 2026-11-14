/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Audacity.Project 1.0

import "."

ProjectsView {
    id: root

    RecentProjectsModel {
        id: recentProjectsModel
    }

    Component.onCompleted: {
        recentProjectsModel.load()
    }

    sourceComponent: projectListComp

    Component {
        id: projectListComp

        DefaultProjectListView {
            anchors.fill: parent

            model: recentProjectsModel
            viewType: root.viewType
            searchText: root.searchText

            backgroundColor: root.backgroundColor
            sideMargin: root.sideMargin

            showNewProjectItem: true

            navigationSection: root.navigationSection
            navigationOrder: root.navigationOrder
            navigationName: "RecentProjects"
            gridAccessibleName: qsTrc("project", "Recent projects grid")
            listAccessibleName: qsTrc("project", "Recent projects list")

            onCreateNewProjectRequested: root.createNewProjectRequested()

            onOpenProjectRequested: function (projectPath, displayName) {
                root.openProjectRequested(projectPath, displayName)
            }

            onOpenCloudProjectRequested: function (projectId) {
                root.openCloudProjectRequested(projectId)
            }
        }
    }
}
