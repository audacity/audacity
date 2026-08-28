/*
 * Audacity: A Digital Audio Editor
 *
 * UI check: a newly created, empty project.
 */

var Home = require("../steps/Home.js");
var Screenshot = require("Audacity.Screenshot");

var testCase = {
    name: "UI check: Empty project",
    description: "Captures a new, empty project for screenshot comparison",
    steps: [
        {
            name: "Let the UI settle",
            func: function () {
                // Past the modules' delayed initialization, 5 s after start
                api.testflow.sleep(6000);
            },
        },
        {
            name: "Create new project",
            func: function () {
                Home.createNewProject();
                // The project page loads asynchronously
                api.testflow.sleep(3000);
            },
        },
        {
            name: "Capture the empty project",
            func: function () {
                if (!Screenshot.save("project-empty")) {
                    api.testflow.error("failed to capture the project page");
                }
            },
        },
    ],
};

function main() {
    api.testflow.setInterval(1000);
    api.testflow.runTestCase(testCase);
}
