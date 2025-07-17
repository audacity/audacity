/*
 * Audacity: A Digital Audio Editor
 */

var Navigation = require("steps/Navigation.js")
var Home = require("steps/Home.js")


var testCase = {
    name: "TC1.6: Real Time Effects Shortcut",
    description: "Tests the Real Time Effects Shortcut key (E)",
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
            name: "Check if Effects are disabled", func: function () {
                Navigation.assertControlsDisabled("TopTool", "MainToolBar")
            }
        },
        {
            name: "Check Controls", func: function () {
                Navigation.assertControlsDisabled("TopTool", "MainToolBar")
            }
        },
        {
            name: "Check PlaybackSection", func: function () {
                Navigation.assertControlsDisabled("PlaybackSection", "ToolBarView")
            }
        },
    ]
}

function main() {
    api.autobot.setInterval(1000)
    api.autobot.runTestCase(testCase)
}
