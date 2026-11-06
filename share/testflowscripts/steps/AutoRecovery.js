/*
 * Audacity: A Digital Audio Editor
 *
 * Helpers to drive the automatic crash recovery dialog through navigation.
 */

var u = require("steps/TestUtils.js");

var DIALOG = "AutoRecoveryDialog";
var PROJECTS = "AutoRecoveryProjects";
var BUTTONS = "AutoRecoveryButtons";

// Questions, e.g. the confirmation before discarding unsaved projects
var QUESTION = "DialogView";
var QUESTION_BUTTONS = "StandardDialog";

// Polls, as dialogs open and close on later event loop iterations
function waitFor(condition, timeoutMs) {
    for (var waited = 0; waited < timeoutMs; waited += 100) {
        if (condition()) {
            return true;
        }
        u.sleep(100);
    }
    return condition();
}

function hasOpenSection(name) {
    var sections = api.navigation.sections();
    for (var i = 0; i < sections.length; ++i) {
        if (sections[i].name === name && sections[i].enabled) {
            return true;
        }
    }
    return false;
}

// Checked before reading the dialog's controls: the navigation API crashes
// when asked for the controls of a section that no longer exists
function isOpen() {
    return hasOpenSection(DIALOG);
}

function expectOpen() {
    if (!isOpen()) {
        u.fail("recovery dialog is not open");
    }
    u.eq(api.navigation.activeSection(), DIALOG, "active navigation section");
}

function expectClosed() {
    if (
        !waitFor(function () {
            return !isOpen();
        }, 3000)
    ) {
        u.fail("recovery dialog is still open");
    }
}

// One checkbox per row of the table
function projectCount() {
    expectOpen();
    var controls = api.navigation.controls(DIALOG, PROJECTS);
    var count = 0;
    for (var i = 0; i < controls.length; ++i) {
        if (controls[i].name === "CheckBox") {
            ++count;
        }
    }
    return count;
}

function expectProjectCount(count) {
    u.eq(projectCount(), count, "number of projects");
}

// Rows are numbered from 0
function toggle(row) {
    expectOpen();
    // Puts the row's checkbox in edit mode, which only takes effect on a later event loop iteration
    if (!api.navigation.triggerControl(DIALOG, PROJECTS, [row + 1, 0])) {
        u.fail("no row " + row);
    }
    u.sleep(300);
    api.navigation.trigger();
    // Lets the buttons follow the new selection
    u.sleep(300);
}

// Clicks the "Select" column header
function invertSelection() {
    expectOpen();
    if (!api.navigation.triggerControl(DIALOG, PROJECTS, [0, 0])) {
        u.fail("no Select header");
    }
    u.sleep(300);
}

function isButtonEnabled(name) {
    expectOpen();
    var controls = api.navigation.controls(DIALOG, BUTTONS);
    for (var i = 0; i < controls.length; ++i) {
        if (controls[i].name === name) {
            return controls[i].enabled;
        }
    }
    u.fail("no button " + name);
}

// Discard and Recover are only enabled when something is selected
function expectSelection(hasSelection) {
    u.eq(
        isButtonEnabled("Discard selected"),
        hasSelection,
        "Discard selected enabled",
    );
    u.eq(
        isButtonEnabled("Recover selected"),
        hasSelection,
        "Recover selected enabled",
    );
}

function click(button) {
    expectOpen();
    if (!api.navigation.triggerControl(DIALOG, BUTTONS, button)) {
        u.fail("cannot click " + button);
    }
}

function isQuestionOpen() {
    return (
        hasOpenSection(QUESTION) && api.navigation.activeSection() === QUESTION
    );
}

function answer(button) {
    if (!waitFor(isQuestionOpen, 3000)) {
        u.fail("no question is open");
    }
    // Its buttons only take navigation a little after it shows
    u.sleep(500);
    if (!api.navigation.triggerControl(QUESTION, QUESTION_BUTTONS, button)) {
        u.fail("cannot answer " + button);
    }
}

module.exports = {
    waitFor: waitFor,
    isOpen: isOpen,
    expectOpen: expectOpen,
    expectClosed: expectClosed,
    projectCount: projectCount,
    expectProjectCount: expectProjectCount,
    toggle: toggle,
    invertSelection: invertSelection,
    isButtonEnabled: isButtonEnabled,
    expectSelection: expectSelection,
    click: click,
    isQuestionOpen: isQuestionOpen,
    answer: answer,
};
