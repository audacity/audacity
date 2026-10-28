/*
 * Audacity: A Digital Audio Editor
 *
 * TC3.2 step 3: a project whose files no longer exist is not offered.
 *
 * @testflow allow-recovery
 */

var u = require("steps/TestUtils.js");
var r = require("steps/AutoRecovery.js");

var testCase = {
    name: "TC3.2.3: Nothing to recover",
    description: "The deleted project is not offered, so the dialog does not show",
    steps: [
        u.step("No recovery dialog", function () {
            r.expectClosed();
        }),
    ],
};

function main() {
    api.testflow.setInterval(1000);
    api.testflow.runTestCase(testCase);
}
