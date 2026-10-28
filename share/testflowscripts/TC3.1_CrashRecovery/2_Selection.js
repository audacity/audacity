/*
 * Audacity: A Digital Audio Editor
 *
 * TC3.1 step 2: the recovery dialog lists the three projects, all selected.
 * Discarding and recovering need a selection; the "Select" header inverts it.
 * Escape closes the dialog.
 *
 * @testflow allow-recovery
 */

var u = require("steps/TestUtils.js");
var r = require("steps/AutoRecovery.js");

var testCase = {
    name: "TC3.1.2: Selection",
    description:
        "All projects are selected at first, Discard and Recover need a selection, the header inverts it",
    steps: [
        u.step(
            "Previous test cases finished with three projects open. Dialog lists the three projects",
            function () {
                r.expectProjectCount(3);
            },
        ),
        u.step("Unselecting two of them leaves a selection", function () {
            r.toggle(0);
            r.toggle(1);
            r.expectSelection(true);
        }),
        u.step("Unselecting the third leaves none", function () {
            r.toggle(2);
            r.expectSelection(false);
        }),
        u.step("The Select header inverts the selection", function () {
            r.invertSelection();
            r.expectSelection(true);
        }),
        u.step("Press ESC", function () {
            api.navigation.escape();
        }),
        u.step("ESC closed the dialog", function () {
            r.expectClosed();
        }),
    ],
};

function main() {
    api.testflow.setInterval(1000);
    api.testflow.runTestCase(testCase);
}
