/*
 * Audacity: A Digital Audio Editor
 */


var Navigation = require("Navigation.js")

module.exports = {
    newMonoTrack: function () {
        api.dispatcher.dispatch("new-mono-track")
    },
    newStereoTrack: function () {
        api.dispatcher.dispatch("new-stereo-track")
    },

}
