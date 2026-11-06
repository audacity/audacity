/*
 * Audacity: A Digital Audio Editor
 *
 * TC3.1 step 8: with every project discarded, startup offers no recovery.
 *
 * @testflow allow-recovery
 */

var u = require("steps/TestUtils.js");
var r = require("steps/AutoRecovery.js");

var testCase = {
    name: "TC3.1.8: Nothing to recover",
    description:
        "Once all projects are recovered or discarded, the dialog does not show",
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
