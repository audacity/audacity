/*
 * Audacity: A Digital Audio Editor
 *
 * TC3.1 step 3: escaping the dialog kept the projects. Skip closes it.
 *
 * @testflow allow-recovery
 */

var u = require("steps/TestUtils.js");
var r = require("steps/AutoRecovery.js");

var testCase = {
    name: "TC3.1.3: Skip",
    description: "ESC kept the projects for this run; Skip closes the dialog",
    steps: [
        u.step("ESC kept the three projects", function () {
            r.expectProjectCount(3);
        }),
        u.step("Press `Skip`", function () {
            r.click("Skip");
        }),
        u.step("Skip closed the dialog", function () {
            r.expectClosed();
        }),
    ],
};

function main() {
    api.testflow.setInterval(1000);
    api.testflow.runTestCase(testCase);
}
