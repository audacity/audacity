/*
 * Audacity: A Digital Audio Editor
 *
 * NOTE The "Advanced Audio Editing" workspace this test was originally
 * written for no longer exists; the available workspaces are Modern
 * (the default), Classic and Music. This test covers the two that
 * TC1.3 does not.
 *
 * NOTE The expected control counts are the shipped workspace defaults, only
 * guaranteed on a fresh profile (CI, or the headless launch configuration).
 * The toolbar composition is persisted UI state: personal customizations, a
 * config saved by an older build, or the playback-meter-in-sidebar preference
 * all change the count on a personal profile.
 */

var Navigation = require("steps/Navigation.js")
var Home = require("steps/Home.js")

var testCase = {
    name: "TC1.4: Modern and Music Workspaces",
    description: "Checks the toolbar control count in the Modern and Music workspaces.",
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
            name: "Switch to the Modern workspace and count the toolbar items", func: function () {
                api.dispatcher.dispatch("command://workspace/select?name=Modern")
                api.testflow.sleep(1500)
                Navigation.assertControlCount("PlaybackSection", "PlaybackToolBar", 34)
            }
        },
        {
            name: "Switch to the Music workspace and count the toolbar items", func: function () {
                api.dispatcher.dispatch("command://workspace/select?name=Music")
                api.testflow.sleep(1500)
                Navigation.assertControlCount("PlaybackSection", "PlaybackToolBar", 49)
            }
        }
    ]
};

function main() {
    api.testflow.setInterval(1000)
    api.testflow.runTestCase(testCase)
}
