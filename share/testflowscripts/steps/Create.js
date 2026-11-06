/*
 * Audacity: A Digital Audio Editor
 */


module.exports = {
    newMonoTrack: function () {
        api.dispatcher.dispatch("command://trackedit/new-mono-track")
    },
    newStereoTrack: function () {
        api.dispatcher.dispatch("command://trackedit/new-stereo-track")
    },

}
