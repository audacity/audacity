/*
 * Audacity: A Digital Audio Editor
 */


module.exports = {
    newMonoTrack: function () {
        api.dispatcher.dispatch("new-mono-track")
    },
    newStereoTrack: function () {
        api.dispatcher.dispatch("new-stereo-track")
    },

}
