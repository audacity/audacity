/*
 * Audacity: A Digital Audio Editor
 *
 * TC3.2 step 1: open a project and leave it open.
 *
 * The runner ends every test case without tearing down the app, so the
 * project is never closed: for the next step, this run crashed.
 */

var u = require("steps/TestUtils.js");

var testCase = {
    name: "TC3.2.1: Open a project",
    description: "Leaves a new project open for the next step",
    steps: [
        u.step("New project", function () {
            u.run("file-new");
            u.sleep(2000);
        }),
    ],
};

function main() {
    api.testflow.setInterval(1000);
    api.testflow.runTestCase(testCase);
}
