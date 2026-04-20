/*
 * Audacity: A Digital Audio Editor
 */

var Home = require("steps/Home.js")
var Create = require("steps/Create.js")

var testCase = {
    name: "TC1.1: Basic Test",
    description: "Just do anything with testflow",
    steps: [
        {
            name: "Close project (if opened) and go to home to start", func: function () {
                api.dispatcher.dispatch("file-close")
                Home.goToHome()
                Create.newMonoTrack()
            }
        },
    ]
}

function main() {
    api.testflow.setInterval(1000)
    api.testflow.runTestCase(testCase)
}
