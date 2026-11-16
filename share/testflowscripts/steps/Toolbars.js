/*
 * Audacity: A Digital Audio Editor
 */

// Playback toolbar controls of each workspace, as shipped
module.exports = {
    common: [
        "command://playback/toggle-play-pause",
        "command://playback/stop",
        "command://record/start",
        "command://playback/rewind-start",
        "command://playback/rewind-end",
        "command://playback/toggle-loop-region",
        "command://projectscene/toggle-clip-gain-automation",
        "command://projectscene/zoom-in",
        "command://projectscene/zoom-out"
    ],
    classicOnly: [
        "command://trackedit/cut",
        "command://trackedit/copy",
        "command://trackedit/paste-default",
        "command://projectscene/zoom-to-fit-project",
        "command://projectscene/zoom-to-selection"
    ],
    modernOnly: [
        "command://trackedit/global-view-spectrogram"
    ],
    musicOnly: [
        "TimeSignature",
        "BPMArrowUpButton",
        "BPMArrowDownButton"
    ]
}
