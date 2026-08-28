/*
 * Audacity: A Digital Audio Editor
 *
 * UI check: the home page on a fresh start.
 */

var Screenshot = require("Audacity.Screenshot");

var testCase = {
    name: "UI check: Home page",
    description: "Captures the home page for screenshot comparison",
    steps: [
        {
            name: "Let the UI settle",
            func: function () {
                // Past the modules' delayed initialization, 5 s after start
                api.testflow.sleep(6000);
            },
        },
        {
            name: "Capture the home page",
            func: function () {
                if (!Screenshot.save("home")) {
                    api.testflow.error("failed to capture the home page");
                }
            },
        },
    ],
};

function main() {
    api.testflow.setInterval(1000);
    api.testflow.runTestCase(testCase);
}
