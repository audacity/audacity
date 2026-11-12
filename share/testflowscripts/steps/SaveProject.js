/*
 * Audacity: A Digital Audio Editor
 *
 * Saves the current project without going through the save dialogs.
 */

var u = require("steps/TestUtils.js");

// `name` ends in .aup4. The file goes in the testflow data directory, and the
// project becomes the most recent one, which is how the next run of a series finds it.
function saveProject(name) {
    api.testflow.saveProject(name);
    // Dispatching a command only queues it
    u.sleep(500);
}

module.exports = {
    saveProject: saveProject,
};
