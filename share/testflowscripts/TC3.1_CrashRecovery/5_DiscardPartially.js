/*
 * Audacity: A Digital Audio Editor
 *
 * TC3.1 step 5: quitting kept the projects. Discarding unsaved projects asks
 * for confirmation; discarding some keeps the dialog open with the others.
 *
 * @testflow allow-recovery
 */

var u = require("steps/TestUtils.js");
var r = require("steps/AutoRecovery.js");

var testCase = {
    name: "TC3.1.5: Discard partially",
    description:
        "Quit kept the projects; discarding one asks first, then keeps the dialog open with the other two",
    steps: [
        u.step("Quit kept the three projects", function () {
            r.expectProjectCount(3);
        }),
        u.step("Select just the last project and press `Discard`", function () {
            r.toggle(0);
            r.toggle(1);
            r.click("Discard selected");
        }),
        u.step("Discarding asks for confirmation: press `No`", function () {
            r.answer("No");
        }),
        u.step("No discarded nothing", function () {
            r.expectProjectCount(3);
        }),
        u.step("Press `Discard` again", function () {
            r.click("Discard selected");
        }),
        u.step("Confirm: Yes", function () {
            r.answer("Yes");
        }),
        u.step("The dialog stays open with the other two", function () {
            r.expectProjectCount(2);
        }),
        u.step("Press `Skip`", function () {
            r.click("Skip");
        }),
    ],
};

function main() {
    api.testflow.setInterval(1000);
    api.testflow.runTestCase(testCase);
}
