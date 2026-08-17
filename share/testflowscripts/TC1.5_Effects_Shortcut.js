/*
 * Audacity: A Digital Audio Editor
 */

var Home = require("steps/Home.js")
var Shortcut = require("steps/Shortcut.js")

// The realtime effects section registers these navigation controls whether
// the panel is shown or hidden; their enabled flags flip with the panel's
// visibility, which makes the count of enabled controls a usable signal.
function effectsPanelState() {
    var controls = api.navigation.controls("AddNewTrackSection", "RealtimeEffectsSectionPanel")
    var enabled = 0
    for (var i = 0; i < controls.length; i++) {
        if (controls[i].enabled) {
            enabled++
        }
    }
    return enabled
}

var hiddenState = -1

var testCase = {
    name: "TC1.5: Real Time Effects Shortcut",
    description: "Tests the Real Time Effects Shortcut key (E)",
    steps: [
        {
            name: "Close project (if opened) and go to home to start", func: function () {
                api.dispatcher.dispatch("file-close")
                Home.goToHome()
            }
        },
        {
            name: "Create new project with a track", func: function () {
                Home.createNewProject()
                // The project page loads asynchronously, and querying a
                // navigation panel before it exists crashes the app
                api.testflow.sleep(2000)
                api.dispatcher.dispatch("new-mono-track")
                api.testflow.sleep(500)
            }
        },
        {
            name: "Hide the effects panel to start from a known state", func: function () {
                // The panel is visible by default on a fresh profile
                api.dispatcher.dispatch("toggle-effects")
                api.testflow.sleep(800)
                hiddenState = effectsPanelState()
            }
        },
        {
            name: "Open the effects panel with the shortcut", func: function () {
                Shortcut.openEffects()
                api.testflow.sleep(800)
                var state = effectsPanelState()
                if (state === hiddenState) {
                    api.testflow.error("Pressing E did not toggle the effects panel (state still " + state + ")")
                }
            }
        }
    ]
}

function main() {
    api.testflow.setInterval(1000)
    api.testflow.runTestCase(testCase)
}
