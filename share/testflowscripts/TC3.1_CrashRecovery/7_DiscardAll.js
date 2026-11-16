/*
 * Audacity: A Digital Audio Editor
 *
 * TC3.1 step 7: the project that was not recovered was kept, next to the
 * recovered one, left behind again. Discarding all projects closes the dialog.
 *
 * @testflow allow-recovery
 */

var u = require("steps/TestUtils.js");
var r = require("steps/AutoRecovery.js");

var testCase = {
    name: "TC3.1.7: Discard all",
    description:
        "The unrecovered project was kept; discarding all closes the dialog",
    steps: [
        // Only the recovered one would be listed if the other had not been kept
        u.step("The projects that were not recovered were kept", function () {
            r.expectProjectCount(2);
        }),
        u.step("Discard them", function () {
            r.click("Discard selected");
        }),
        u.step("Confirm", function () {
            r.answer("Yes");
        }),
        u.step("The dialog closed", function () {
            r.expectClosed();
        }),
    ],
};

function main() {
    api.testflow.setInterval(1000);
    api.testflow.runTestCase(testCase);
}
