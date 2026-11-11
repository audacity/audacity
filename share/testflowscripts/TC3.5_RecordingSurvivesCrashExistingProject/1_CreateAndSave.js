/*
 * Audacity: A Digital Audio Editor
 *
 * TC3.5 step 1: create a project with a track, and save it.
 */

var u = require("steps/TestUtils.js");
var s = require("steps/SaveProject.js");

var testCase = {
    name: "TC3.5.1: Create and save a project",
    description: "Saves a project with one empty track",
    steps: [
        u.step("Create a project with a track", function () {
            u.run("command://project/new");
            u.sleep(2000);
            u.run("command://trackedit/new-mono-track");
            // Dispatching a command only queues it
            u.sleep(500);
            u.eq(u.trackCount(), 1, "track count");
        }),
        u.step("Save it", function () {
            s.saveProject("TC3.5_RecordingSurvivesCrashExistingProject.aup4");
        }),
        u.step("Close it", function () {
            u.run("command://project/close");
            u.sleep(1000);
            u.eq(u.trackCount(), 0, "track count");
        }),
    ],
};

function main() {
    api.testflow.setInterval(1000);
    api.testflow.runTestCase(testCase);
}
