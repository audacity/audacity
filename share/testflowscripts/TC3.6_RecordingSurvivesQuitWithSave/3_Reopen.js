/*
 * Audacity: A Digital Audio Editor
 *
 * TC3.6 step 3: the saved project has the recording.
 */

var u = require("steps/TestUtils.js");
var Home = require("steps/Home.js");
var rec = require("steps/Recording.js");

var testCase = {
    name: "TC3.6.3: Reopen the project",
    description: "The project saved when quitting has its track and the recorded clip",
    steps: [
        u.step("Open the saved project", function () {
            Home.goToHome();
            u.sleep(1000);
            if (!Home.openMostRecentProject()) {
                u.fail("no recent project");
            }
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
