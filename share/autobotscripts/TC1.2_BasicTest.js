/*
 * Audacity: A Digital Audio Editor
 */

var Navigation = require("steps/Navigation.js")
var Home = require("steps/Home.js")
var ClipNavigation = require("steps/ClipNavigation.js")

var testCase = {
    name: "TC1.3: Basic Test with Helper Functions, Creating Projects",
    description: "Test the Project Open Functions",
    steps: [
        {name: "Close project (if opened) and go to home to start", func: function() {
            api.dispatcher.dispatch("file-close")
            Home.goToHome()
        }},
        {name: "Open Recent Project", func: function() {
            Home.openLastProject()
        }}
    ]
};

function main()
{
    api.autobot.setInterval(1000)
    api.autobot.runTestCase(testCase)
}
