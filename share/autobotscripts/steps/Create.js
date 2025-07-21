/*
 * Audacity: A Digital Audio Editor
 */


var Navigation = require("Navigation.js")

module.exports = {
    newMonoTrack: function () {
        // This function creates a new track.
        api.shortcuts.activate("Alt+T")
        api.navigation.trigger()
    },
    newStereoTrack: function () {
        api.shortcut.key("Alt+T")
        api.navigation.down()
        api.navigation.trigger()
    },

}
