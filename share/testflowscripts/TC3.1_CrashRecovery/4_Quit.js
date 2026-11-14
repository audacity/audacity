/*
 * Audacity: A Digital Audio Editor
 *
 * TC3.1 step 4: skipping kept the projects. Quit Audacity quits.
 *
 * @testflow allow-recovery
 */

var u = require("steps/TestUtils.js");
var r = require("steps/AutoRecovery.js");

var testCase = {
    name: "TC3.1.4: Quit",
    description: "Skip kept the projects for this run; Quit Audacity quits",
    steps: [
        u.step("Skip kept the three projects", function () {
            r.expectProjectCount(3);
        }),
        u.step("Press `Quit Audacity`", function () {
            r.click("Quit Audacity");
        }),
        // Quitting ends the test case as passed, before this step
        u.step("Audacity has quit", function () {
            u.fail("Audacity did not quit");
        }),
    ],
};

function main() {
    api.testflow.setInterval(1000);
    api.testflow.runTestCase(testCase);
}
