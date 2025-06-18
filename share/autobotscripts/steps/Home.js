/*
 * Audacity: A Digital Audio Editor
 */

var Navigation = require("Navigation.js")

module.exports = {
    goToHome: function()
    {
        Navigation.triggerControl("TopTool", "MainToolBar", "Home")
    },

    openLastProject: function()
    {
        Navigation.goToControl("RecentScores", "RecentScoresGrid", "New score")
        api.navigation.right()
        api.navigation.trigger()
    }
}
