/*
 * Audacity: A Digital Audio Editor
 *
 * TC3.3 step 2: the project saved by the previous run opens from the recent
 * projects, with its track.
 */

var u = require("steps/TestUtils.js");
var Home = require("steps/Home.js");

var testCase = {
    name: "TC3.3.2: Reopen the saved project",
    description: "The saved project is in the recent projects and has its track",
    steps: [
        u.step("No project is open", function () {
            u.eq(u.trackCount(), 0, "track count");
        }),
        u.step("Open the most recent project", function () {
            Home.goToHome();
            u.sleep(1000);
            if (!Home.openMostRecentProject()) {
                u.fail("no recent project");
            }
        }),
        u.step("It has its track", function () {
            u.waitFor(function () {
                return u.trackCount() === 1;
            }, 5000);
            u.eq(u.trackCount(), 1, "track count");
        }),
    ],
};

function main() {
    api.testflow.setInterval(1000);
    api.testflow.runTestCase(testCase);
}
