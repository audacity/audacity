/*
 * Audacity: A Digital Audio Editor
 *
 * TC3.6 step 2: reopen the saved project, record on a new track, and quit
 * while recording, answering Save when asked about the unsaved changes.
 *
 * @testflow expect-quit 3
 */

var u = require("steps/TestUtils.js");
var r = require("steps/AutoRecovery.js");
var Home = require("steps/Home.js");
var rec = require("steps/Recording.js");

var testCase = {
    name: "TC3.6.2: Record, and quit saving",
    description: "Quitting while recording asks about saving; Save keeps the recording",
    steps: [
        u.step("Open the saved project", function () {
            Home.goToHome();
            u.sleep(1000);
            if (!Home.openMostRecentProject()) {
                u.fail("no recent project");
            }
            u.waitFor(function () {
                return u.trackCount() === 1;
            }, 5000);
            u.eq(u.trackCount(), 1, "track count");
        }),
        u.step("Record, and quit without stopping", function () {
            rec.recordOnNewTrack(2000);
            u.run("command://app/quit");
        }),
        u.step("Asked about saving, press Save", function () {
            r.answer("Save");
        }),
        // Quitting ends the test case as passed, before this step
        u.step("Audacity has quit", function () {
            u.waitFor(function () {
                return false;
            }, 5000);
            u.fail("Audacity did not quit");
        }),
    ],
};

function main() {
    api.testflow.setInterval(1000);
    api.testflow.runTestCase(testCase);
}
