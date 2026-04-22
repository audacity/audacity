/*
 * Audacity: A Digital Audio Editor
 *
 * TC2.1: Cut/Delete Action Variants
 *
 * Verifies behavioral differences between cut/delete modes:
 *   - Leave gap:        audio removed, gap stays, clips don't move
 *   - Per-clip ripple:  audio removed, clips trimmed, no shift
 *   - Per-track ripple: gap closed by shifting clips on selected tracks
 *   - All-tracks ripple: gap closed on ALL tracks regardless of selection
 *
 * Setup: two tracks with 3s chirps, split into clips [0,1] [1,2] [2,3].
 */

var Home = require("steps/Home.js")
var Create = require("steps/Create.js")
var u = require("steps/TestUtils.js")

function chirp(track, dur, freqLo, freqHi) {
    u.selectTrack(track)
    u.select(0, dur)
    u.effect("Chirp", { StartFreq: freqLo, EndFreq: freqHi, StartAmp: 0.8, EndAmp: 0.1 })
}

// setup state

var initial = {}

function verifyTrackMatches(label, trackIndex, expected) {
    var now = u.clips(trackIndex)
    u.eq(now.length, expected.length, label + " track " + trackIndex + " clip count")
    for (var i = 0; i < expected.length && i < now.length; i++) {
        u.approx(now[i].start, expected[i].start, label + " track " + trackIndex + " clip " + i + " start")
        u.approx(now[i].end,   expected[i].end,   label + " track " + trackIndex + " clip " + i + " end")
    }
}

function verifyUndo(label) {
    u.undo(); u.sleep(500)
    verifyTrackMatches(label + " undo", 0, initial.t0)
    verifyTrackMatches(label + " undo", 1, initial.t1)
}

function testAction(name, action, setup, verify) {
    return u.step(name, function () {
        setup()
        u.sleep(100)
        u.log("before " + name)

        u.run(action)
        u.sleep(500)
        u.log("after  " + name)

        verify()
        verifyUndo(name)
    })
}

// steps

var steps = []

// 1. Setup
steps.push(u.step("Setup: two tracks, 3 clips each", function () {
    u.run("file-close")
    Home.goToHome()
    Home.createNewProject()
    Create.newMonoTrack()
    Create.newMonoTrack()
    u.sleep(500)

    chirp(0, 3.0, 220, 880)  ; u.sleep(200)
    chirp(1, 3.0, 440, 1760) ; u.sleep(200)

    u.selectAllTracks()
    u.select(1.0, 2.0) ; u.sleep(100)
    u.run("split")      ; u.sleep(200)

    u.eq(u.clipCount(0), 3, "track 0 clips after split")
    u.eq(u.clipCount(1), 3, "track 1 clips after split")

    initial.t0 = u.clips(0)
    initial.t1 = u.clips(1)
    u.log("setup")
}))

// 2. Leave gap: middle clip removed, third clip stays at 2.0
var leaveGap = ["cut-leave-gap", "delete-leave-gap"]
for (var i = 0; i < leaveGap.length; i++) {
    (function (a) {
        steps.push(testAction(a, a,
            function () { u.selectAllTracks(); u.select(1.0, 2.0) },
            function () {
                u.eq(u.clipCount(0), 2, "track 0 clips")
                u.eq(u.clipCount(1), 2, "track 1 clips")
                u.approx(u.clips(0)[1].start, 2.0, "track 0 last clip stays")
                u.approx(u.clips(1)[1].start, 2.0, "track 1 last clip stays")
            }
        ))
    })(leaveGap[i])
}

// 3. Leave gap, cross-boundary [0.5, 1.5]: third clip untouched
for (var i = 0; i < leaveGap.length; i++) {
    (function (a) {
        steps.push(testAction(a + " (cross)", a,
            function () { u.selectAllTracks(); u.select(0.5, 1.5) },
            function () {
                u.approx(u.clips(0)[0].end, 0.5, "first clip trimmed")
                var c = u.clips(0)
                u.approx(c[c.length - 1].start, 2.0, "last clip stays")
            }
        ))
    })(leaveGap[i])
}

// 4. Per-clip ripple [0.5, 1.5]: trimmed but NOT shifted
var perClip = ["cut-per-clip-ripple", "delete-per-clip-ripple"]
for (var i = 0; i < perClip.length; i++) {
    (function (a) {
        steps.push(testAction(a + " (cross)", a,
            function () { u.selectAllTracks(); u.select(0.5, 1.5) },
            function () {
                u.approx(u.clips(0)[0].end, 0.5, "first clip trimmed")
                var c = u.clips(0)
                u.approx(c[c.length - 1].start, 2.0, "last clip NOT shifted")
            }
        ))
    })(perClip[i])
}

// 5. Per-track ripple: both tracks shift (both selected)
var perTrack = ["cut-per-track-ripple", "delete-per-track-ripple"]
for (var i = 0; i < perTrack.length; i++) {
    (function (a) {
        steps.push(testAction(a, a,
            function () { u.selectAllTracks(); u.select(1.0, 2.0) },
            function () {
                u.eq(u.clipCount(0), 2, "track 0 clips")
                u.approx(u.clips(0)[1].start, 1.0, "track 0 shifted")
                u.approx(u.clips(1)[1].start, 1.0, "track 1 shifted")
            }
        ))
    })(perTrack[i])
}

// 6. Per-track ripple, cross-boundary [0.5, 1.5]: shifts (unlike per-clip)
for (var i = 0; i < perTrack.length; i++) {
    (function (a) {
        steps.push(testAction(a + " (cross)", a,
            function () { u.selectAllTracks(); u.select(0.5, 1.5) },
            function () {
                u.approx(u.clips(0)[0].end, 0.5, "first clip trimmed")
                var c = u.clips(0)
                u.approx(c[c.length - 1].start, 1.0, "last clip shifted to 1.0")
            }
        ))
    })(perTrack[i])
}

// 7. Per-track ripple, single track: only track 0 shifts
for (var i = 0; i < perTrack.length; i++) {
    (function (a) {
        steps.push(testAction(a + " (track 0 only)", a,
            function () { u.selectTrack(0); u.select(1.0, 2.0) },
            function () {
                u.eq(u.clipCount(0), 2, "track 0 clips")
                u.approx(u.clips(0)[1].start, 1.0, "track 0 shifted")
                u.eq(u.clipCount(1), 3, "track 1 untouched")
                u.approx(u.clips(1)[2].start, initial.t1[2].start, "track 1 stays")
            }
        ))
    })(perTrack[i])
}

// 8. All-tracks ripple, single track selected: BOTH tracks shift
var allTracks = ["cut-all-tracks-ripple", "delete-all-tracks-ripple"]
for (var i = 0; i < allTracks.length; i++) {
    (function (a) {
        steps.push(testAction(a + " (track 0 only)", a,
            function () { u.selectTrack(0); u.select(1.0, 2.0) },
            function () {
                u.eq(u.clipCount(0), 2, "track 0 clips")
                u.approx(u.clips(0)[1].start, 1.0, "track 0 shifted")
                u.eq(u.clipCount(1), 2, "track 1 clips")
                u.approx(u.clips(1)[1].start, 1.0, "track 1 ALSO shifted")
            }
        ))
    })(allTracks[i])
}

// run

var testCase = {
    name: "TC2.1: Cut/Delete Action Variants",
    description: "Verifies gap, per-clip, per-track, and all-tracks ripple behavior",
    steps: steps
}

function main() {
    api.testflow.setInterval(1000)
    api.testflow.runTestCase(testCase)
}
