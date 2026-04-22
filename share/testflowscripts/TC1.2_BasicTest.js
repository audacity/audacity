/*
 * Audacity: A Digital Audio Editor
 */

var Home = require("steps/Home.js")

var testCase = {
    name: "TC1.3: Basic Test with Helper Functions, Creating Projects",
    description: "Test the Project Open Functions",
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
        }
    ]
};

function main() {
    api.testflow.setInterval(1000)
    api.testflow.runTestCase(testCase)
}
