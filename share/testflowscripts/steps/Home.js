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
    // Create new project
    createNewProject: function () {
        Navigation.triggerControl("RecentProjects", "RecentProjectsGrid", "New project")
    }
}
