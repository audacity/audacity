/*
 * Audacity: A Digital Audio Editor
 *
 * Common utilities for testflow scripts.
 */

var Project = require("Audacity.Project")
var Effects = require("Audacity.Effects")

var EPSILON = 0.001

// dispatch helpers

function select(start, end) {
    api.dispatcher.dispatch("action://trackedit/set-selection?start=" + start + "&end=" + end)
}
function selectAllTracks() {
    api.dispatcher.dispatch("select-all-tracks")
}
function selectTrack(t) {
    api.dispatcher.dispatch("action://trackedit/select-track?trackIndex=" + t)
}
function run(action)   { api.dispatcher.dispatch(action) }
function undo()        { run("action://undo") }
function sleep(ms)     { api.testflow.sleep(ms) }
function fail(msg)     { api.testflow.error(msg) }

function waitUntil(pred, timeoutMs, pollMs) {
    timeoutMs = timeoutMs || 120000 // TODO must be > than the validation timeout. How do we keep this in sync?
    pollMs = pollMs || 20
    for (var waited = 0; waited <= timeoutMs; waited += pollMs) {
        if (pred()) return
        sleep(pollMs)
    }
    fail("waitUntil: timed out after " + timeoutMs + "ms")
}

// Wait until an effect with the given title is validated and loadable. On a
// blank-config first run a third-party plugin is validated in the background,
// so its title only appears once validation finishes; call this before
// applying such a plugin by title.
function waitForEffect(title, timeoutMs) {
    waitUntil(function () { return Effects.isAvailable(title) }, timeoutMs)
}

function effect(id, params) {
    var q = "action://effects/apply?effectId=" + id
    for (var k in params) { q += "&" + k + "=" + params[k] }
    api.dispatcher.dispatch(q)
    // Applying an effect is asynchronous (validate-on-first-use then apply);
    // wait for it to finish so the next step sees the result.
    waitUntil(function () { return !Effects.isApplying() })
}

// project queries

function clips(track)     { return Project.clipsOnTrack(track) }
function clipCount(track) { return Project.clipCount(track) }
function trackCount()     { return Project.trackCount() }
function totalTime()      { return Project.totalTime() }

// assertions

function eq(a, b, msg) {
    if (a !== b) fail(msg + " (expected " + b + ", got " + a + ")")
}
function approx(a, b, msg) {
    if (isNaN(a)) fail(msg + " (got NaN, likely bad index or missing project)")
    if (Math.abs(a - b) > EPSILON) fail(msg + " (expected ~" + b.toFixed(3) + ", got " + a.toFixed(3) + ")")
}

// logging

function log(label) {
    for (var t = 0; t < Project.trackCount(); t++) {
        var c = clips(t), s = ""
        for (var i = 0; i < c.length; i++) s += "[" + c[i].start.toFixed(2) + ".." + c[i].end.toFixed(2) + "] "
        api.log.info("  " + label + " track[" + t + "]: " + s)
    }
}

// test step builder

function step(name, fn) { return { name: name, func: fn } }

module.exports = {
    select: select,
    selectAllTracks: selectAllTracks,
    selectTrack: selectTrack,
    run: run,
    undo: undo,
    sleep: sleep,
    fail: fail,
    waitUntil: waitUntil,
    waitForEffect: waitForEffect,
    effect: effect,
    clips: clips,
    clipCount: clipCount,
    trackCount: trackCount,
    totalTime: totalTime,
    eq: eq,
    approx: approx,
    log: log,
    step: step,
    EPSILON: EPSILON
}
