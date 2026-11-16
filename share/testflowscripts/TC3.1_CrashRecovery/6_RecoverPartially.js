/*
 * Audacity: A Digital Audio Editor
 *
 * TC3.1 step 6: the discarded project is gone. Recovering one of the two
 * opens it; the other is kept for the next run.
 *
 * @testflow allow-recovery
 */

var u = require("steps/TestUtils.js");
var r = require("steps/AutoRecovery.js");

var testCase = {
    name: "TC3.1.6: Recover partially",
    description:
        "Recovering one project opens it, with its track",
    steps: [
        u.step("The discarded project is gone", function () {
            r.expectProjectCount(2);
        }),
        u.step("No project is open", function () {
            u.eq(u.trackCount(), 0, "track count");
        }),
        u.step("Recover the first project, the one with a track", function () {
            r.toggle(1);
            r.click("Recover selected");
        }),
        u.step("It is open, with its track", function () {
            r.expectClosed();
            r.waitFor(function () {
                return u.trackCount() === 1;
            }, 3000);
            u.eq(u.trackCount(), 1, "track count");
        }),
        // Left open, it is left behind again, like any project open when the runner ends
    ],
};

function main() {
    api.testflow.setInterval(1000);
    api.testflow.runTestCase(testCase);
}
