/*
 * Audacity: A Digital Audio Editor
 *
 * TC3.2 step 2: the project is offered for recovery. Its files are then
 * deleted, as a user could do between two runs.
 *
 * @testflow allow-recovery
 */

var u = require("steps/TestUtils.js");
var r = require("steps/AutoRecovery.js");

// Only in a throwaway profile under the temporary directory, as given to series
// by run_testflow.sh, never in the user's own
var DELETE_UNSAVED_PROJECTS =
    'case "$XDG_DATA_HOME" in "${TMPDIR:-/tmp}"/?*) ;; *) exit 1 ;; esac; ' +
    'rm "$XDG_DATA_HOME"/Audacity/*/SessionData/*.aup4unsaved*';

var testCase = {
    name: "TC3.2.2: Delete the project",
    description: "Deletes the files of the project offered for recovery",
    steps: [
        u.step("The project is offered", function () {
            r.expectProjectCount(1);
        }),
        u.step("Delete its files", function () {
            u.eq(api.process.execute("sh", ["-c", DELETE_UNSAVED_PROJECTS]), 0, "exit code of deleting");
        }),
        u.step("Skip", function () {
            r.click("Skip");
        }),
    ],
};

function main() {
    api.testflow.setInterval(1000);
    api.testflow.runTestCase(testCase);
}
