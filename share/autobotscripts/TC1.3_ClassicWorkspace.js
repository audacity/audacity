/*
 * Audacity: A Digital Audio Editor
 */

var Navigation = require("steps/Navigation.js")
var Home = require("steps/Home.js")

var testCase = {
    name: "TC1.3: Classic Workspace",
    description: "Checks the amount of controls available in the toolbar if Classic Workspace is enabled.",
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
                var controls = api.navigation.controls("PlaybackSection", "ToolBarView")
                if (controls.length !== 17) {
                    api.autobot.error("Control count is not 17")
                }
            }
        }
    ]
}

function main() {
    api.autobot.setInterval(1000)
    api.autobot.runTestCase(testCase)
}
