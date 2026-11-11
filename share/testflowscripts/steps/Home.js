/*
 * Audacity: A Digital Audio Editor
 */

var Navigation = require("Navigation.js")

module.exports = {
    // Go to main home screen
    goToHome: function () {
        Navigation.triggerControl("TopTool", "MainToolBar", "Home")
    },
    // Open first last project, create new project if no projects are available.
    openLastProject: function () {
        Navigation.goToControl("RecentProjects", "RecentProjectsGrid", "New project")
        api.navigation.right()
        api.navigation.trigger()
    },
    // Open the most recent project; false if there is none
    openMostRecentProject: function () {
        // Row 0 is "New project". Row 1 is the most recent project: its tile, then its menu
        return api.navigation.triggerControl("RecentProjects", "RecentProjectsGrid", [1, 0])
    },
    // Create new project
    createNewProject: function () {
        Navigation.triggerControl("RecentProjects", "RecentProjectsGrid", "New project")
    }
}
