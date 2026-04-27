/*
 * Audacity: A Digital Audio Editor
 */

var Home = require("steps/Home.js")
var ClipNavigation = require("steps/ClipNavigation.js")

var testCase = {
    name: "TC1.2: Basic Test with Helper Functions",
    description: "Select The Clip, Move the Clip",
    steps: [
        {
            name: "Close project (if opened) and go to home to start", func: function () {
                api.dispatcher.dispatch("file-close")
                Home.goToHome()
            }
        },
        {
            name: "Open Recent Project", func: function () {
                Home.openLastProject()
            }
        },
        {
            name: "Enter Clip Editing mode", func: function () {
                ClipNavigation.enterClip()
            }
        }
    ]
}

function main() {
    api.testflow.setInterval(1000)
    api.testflow.runTestCase(testCase)
}
