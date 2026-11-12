/*
 * Audacity: A Digital Audio Editor
 *
 * TC3.4 step 2: the project is recovered with what was recorded before the crash.
 *
 * @testflow allow-recovery
 */

var u = require("steps/TestUtils.js");
var r = require("steps/AutoRecovery.js");
var rec = require("steps/Recording.js");

var testCase = {
    name: "TC3.4.2: Recover the recording",
    description: "The recovered project has the recorded clip",
    steps: [
        u.step("The project is offered", function () {
            r.expectProjectCount(1);
        }),
        u.step("Recover it", function () {
            r.click("Recover selected");
            r.expectClosed();
            u.waitFor(function () {
                return u.trackCount() === 1;
            }, 5000);
            u.eq(u.trackCount(), 1, "track count");
        }),
        u.step("The recording is there", function () {
            rec.expectRecordedClip(0);
        }),
    ],
};

function main() {
    api.testflow.setInterval(1000);
    api.testflow.runTestCase(testCase);
}
