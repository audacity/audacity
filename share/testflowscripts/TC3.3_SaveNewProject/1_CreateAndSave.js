/*
 * Audacity: A Digital Audio Editor
 *
 * TC3.3 step 1: create a new project with a track and save it.
 */

var u = require("steps/TestUtils.js");
var s = require("steps/SaveProject.js");

var testCase = {
    name: "TC3.3.1: Create and save a new project",
    description: "Saves a new project with one track",
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
            s.saveProject("TC3.3_SaveNewProject.aup4");
        }),
    ],
};

function main() {
    api.testflow.setInterval(1000);
    api.testflow.runTestCase(testCase);
}
