/*
 * Audacity: A Digital Audio Editor
 */

var Navigation = require("steps/Navigation.js")
var Home = require("steps/Home.js")

var testCase = {
    name: "TC1.4: Advanced Workspace",
    description: "Checks the amount of controls available in the toolbar if Advanced Audio Ediitng Workspace is enabled.",
    steps: [
        {
            name: "Close project (if opened) and go to home to start", func: function () {
                api.dispatcher.dispatch("file-close")
                Home.goToHome()
            }
        },
        {
            name: "Create new project", func: function () {
                Home.createNewProject()
            }
        },
        {
            name: "Count the Workspace toolbar items", func: function () {
                Navigation.assertControlsCount("PlaybackSection", "ToolBarView", 18)
            }
        }
    ]
};

function main() {
    api.autobot.setInterval(1000)
    api.autobot.runTestCase(testCase)
}
