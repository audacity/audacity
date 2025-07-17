/*
 * Audacity: A Digital Audio Editor
 */

var Home = require("steps/Home.js")
var Shortcut = require("steps/Shortcut.js")

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
            name: "Create new project", func: function () {
                Home.createNewProject()
            }
        },
        {
            name: "Open Real Time Effects Panel", func: function () {
                Shortcut.openEffects()
            }
        },
        {
            name: "Check if the Effects are enabled", func: function () {
                var controls = api.navigation.controls("MasterEffectsSection", "Master effects bypass")
                if (controls[0].enabled === false) {
                    api.autobot.error("Effects Panel is not enabled.")
                }
            }
        },
    ]
}

function main() {
    api.autobot.setInterval(1000)
    api.autobot.runTestCase(testCase)
}
