/*
 * Audacity: A Digital Audio Editor
 */

var Home = require("steps/Home.js")
var Shortcut = require("steps/Shortcut.js")

// The controls exist whether the panel is shown or not, but their enabled
// flags follow its visibility, so the count of enabled ones tracks the panel
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

var stateBeforeShortcut = -1

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
                // Querying a navigation panel before the project page exists crashes the app
                api.testflow.sleep(2000)
                api.dispatcher.dispatch("new-mono-track")
                api.testflow.sleep(500)
            }
        },
        {
            name: "Read the effects panel state", func: function () {
                stateBeforeShortcut = effectsPanelState()
            }
        },
        {
            name: "Toggle the effects panel with the shortcut", func: function () {
                Shortcut.toggleEffects()
                api.testflow.sleep(800)
                var state = effectsPanelState()
                if (state === stateBeforeShortcut) {
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
