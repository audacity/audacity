/*
Helper functions that allow to enter Clip Navigation Mode.
 */

var Navigation = require("Navigation.js")

module.exports = {
    enterClip: function()
    {
        // This Function selects the first clip available in the project.
        Navigation.goToControl("TopTool", "MainToolBar", "Home")
        api.navigation.nextPanel()
        api.navigation.nextPanel()
        api.navigation.nextPanel()
        api.navigation.nextPanel()
    }
}
