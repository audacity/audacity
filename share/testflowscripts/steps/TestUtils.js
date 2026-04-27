/*
 * Audacity: A Digital Audio Editor
 *
 * Common utilities for testflow scripts.
 */

var Project = require("MuseApi.Project")

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

function effect(id, params) {
    var q = "action://effects/apply?effectId=" + id
    for (var k in params) { q += "&" + k + "=" + params[k] }
    api.dispatcher.dispatch(q)
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
