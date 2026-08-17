/*
 * Audacity: A Digital Audio Editor
 *
 * NOTE The expected control count is the shipped workspace default, only
 * guaranteed on a fresh profile (CI, or the headless launch configuration).
 * See TC1.4 for details.
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
                // The project page loads asynchronously, and querying a
                // navigation panel before it exists crashes the app
                api.testflow.sleep(2000)
            }
        },
        {
            name: "Switch to the Classic workspace", func: function () {
                api.dispatcher.dispatch("command://workspace/select?name=Classic")
                api.testflow.sleep(1500)
            }
        },
        {
            name: "Count the toolbar items", func: function () {
                Navigation.assertControlCount("PlaybackSection", "PlaybackToolBar", 43)
            }
        }
    ]
}

function main() {
    api.testflow.setInterval(1000)
    api.testflow.runTestCase(testCase)
}
