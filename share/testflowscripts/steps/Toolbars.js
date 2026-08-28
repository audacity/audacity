/*
 * Audacity: A Digital Audio Editor
 */

// Playback toolbar controls of each workspace, as shipped
module.exports = {
    common: [
        "action://playback/toggle-play-pause",
        "action://playback/stop",
        "action://record/start",
        "action://playback/rewind-start",
        "action://playback/rewind-end",
        "toggle-loop-region",
        "clip-gain",
        "zoom-in",
        "zoom-out"
    ],
    classicOnly: [
        "action://trackedit/cut",
        "action://trackedit/copy",
        "action://trackedit/paste-default",
        "zoom-to-fit-project",
        "zoom-to-selection"
    ],
    modernOnly: [
        "action://trackedit/global-view-spectrogram"
    ],
    musicOnly: [
        "TimeSignature",
        "BPMArrowUpButton",
        "BPMArrowDownButton"
    ]
}
