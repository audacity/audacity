/*
 * Audacity: A Digital Audio Editor
 *
 * TC3.5 step 2: reopen the saved project, record on a new track, and crash
 * while recording.
 *
 * The runner ends every test case without tearing down the app: for the next
 * step, this run crashed in the middle of the recording.
 */

var u = require("steps/TestUtils.js");
var Home = require("steps/Home.js");
var rec = require("steps/Recording.js");

var testCase = {
    name: "TC3.5.2: Reopen, record and crash",
    description: "Crashes while recording in a saved project",
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
        u.step("Record, and crash without stopping", function () {
            rec.recordOnNewTrack(2000);
            u.eq(u.trackCount(), 2, "track count");
        }),
    ],
};

function main() {
    api.testflow.setInterval(1000);
    api.testflow.runTestCase(testCase);
}
