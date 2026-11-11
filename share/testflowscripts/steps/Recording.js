/*
 * Audacity: A Digital Audio Editor
 *
 * Recording steps. On CI the input is ALSA's null device, so what is recorded is
 * silence: only clip lengths can be checked.
 */

var u = require("steps/TestUtils.js");

// Shortest recorded clip that counts as "the recording is there"
var MIN_CLIP_SECONDS = 0.05;

// Starts recording on a new track and lets it run for `ms`, without stopping it
function recordOnNewTrack(ms) {
    u.run("command://record/on-new-track");
    u.sleep(ms);
}

// The track has a clip at least MIN_CLIP_SECONDS long
function expectRecordedClip(track) {
    var clips = u.clips(track);
    if (clips.length === 0) {
        u.fail("no clip on track " + track);
    }
    var duration = clips[0].end - clips[0].start;
    api.log.info("recorded clip on track " + track + ": " + duration.toFixed(3) + " s");
    if (!(duration >= MIN_CLIP_SECONDS)) {
        u.fail("recorded clip on track " + track + " is " + duration.toFixed(3) + " s long");
    }
}

module.exports = {
    recordOnNewTrack: recordOnNewTrack,
    expectRecordedClip: expectRecordedClip,
};
