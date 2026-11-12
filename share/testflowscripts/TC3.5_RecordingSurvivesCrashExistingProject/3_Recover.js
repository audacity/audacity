/*
 * Audacity: A Digital Audio Editor
 *
 * TC3.5 step 3: the saved project is recovered with what was recorded before
 * the crash.
 *
 * @testflow allow-recovery
 */

var u = require("steps/TestUtils.js");
var r = require("steps/AutoRecovery.js");
var rec = require("steps/Recording.js");

var testCase = {
    name: "TC3.5.3: Recover the recording",
    description: "The recovered project has its track and the recorded clip",
    steps: [
        u.step("The project is offered", function () {
            r.expectProjectCount(1);
        }),
        u.step("Recover it", function () {
            r.click("Recover selected");
            r.expectClosed();
            u.waitFor(function () {
                return u.trackCount() === 2;
            }, 5000);
            u.eq(u.trackCount(), 2, "track count");
        }),
        u.step("The recording is there", function () {
            rec.expectRecordedClip(1);
        }),
    ],
};

function main() {
    api.testflow.setInterval(1000);
    api.testflow.runTestCase(testCase);
}
