/*
 * Audacity: A Digital Audio Editor
 *
 * TC3.4 step 1: record in a new project, and crash while recording.
 *
 * The runner ends every test case without tearing down the app: for the next
 * step, this run crashed in the middle of the recording.
 */

var u = require("steps/TestUtils.js");
var rec = require("steps/Recording.js");

var testCase = {
    name: "TC3.4.1: Record and crash",
    description: "Crashes while recording in a new project",
    steps: [
        u.step("New project", function () {
            u.run("command://project/new");
            u.sleep(2000);
        }),
        u.step("Record, and crash without stopping", function () {
            rec.recordOnNewTrack(2000);
            u.eq(u.trackCount(), 1, "track count");
        }),
    ],
};

function main() {
    api.testflow.setInterval(1000);
    api.testflow.runTestCase(testCase);
}
