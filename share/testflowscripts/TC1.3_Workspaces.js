/*
 * Audacity: A Digital Audio Editor
 *
 * NOTE Needs a fresh profile: a customised toolbar has whatever the user put there.
 */

var Navigation = require("steps/Navigation.js")
var Home = require("steps/Home.js")
var Toolbars = require("steps/Toolbars.js")

function checkWorkspace(name, expected, unexpected) {
    api.dispatcher.dispatch("command://workspace/select?name=" + name)
    api.testflow.sleep(1500)
    Navigation.assertControlsPresent("PlaybackSection", "PlaybackToolBar", Toolbars.common.concat(expected))
    Navigation.assertControlsAbsent("PlaybackSection", "PlaybackToolBar", unexpected)
}

var testCase = {
    name: "TC1.3: Workspaces",
    description: "Checks the playback toolbar controls of each workspace.",
    steps: [
        {
            name: "Create new project", func: function () {
                api.dispatcher.dispatch("file-close")
                Home.goToHome()
                Home.createNewProject()
                // Querying a navigation panel before the project page exists crashes the app
                api.testflow.sleep(2000)
            }
        },
        {
            name: "Classic workspace", func: function () {
                checkWorkspace("Classic", Toolbars.classicOnly, Toolbars.modernOnly.concat(Toolbars.musicOnly))
            }
        },
        {
            name: "Modern workspace", func: function () {
                checkWorkspace("Modern", Toolbars.modernOnly, Toolbars.classicOnly.concat(Toolbars.musicOnly))
            }
        },
        {
            name: "Music workspace", func: function () {
                checkWorkspace("Music", Toolbars.musicOnly, Toolbars.classicOnly.concat(Toolbars.modernOnly))
            }
        }
    ]
}

function main() {
    api.testflow.runTestCase(testCase)
}
