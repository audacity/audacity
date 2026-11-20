/*
* Audacity: A Digital Audio Editor
*/
#include "videosyncclock.h"

#include <algorithm>
#include <cmath>

using namespace au::video;

namespace {
//! The sound card and the CPU clock are independent oscillators, but only just:
//! anything outside a few parts per thousand is a measurement artefact rather
//! than a real rate difference, and believing it would make the picture race.
constexpr double MIN_RATIO = 0.995;
constexpr double MAX_RATIO = 1.005;

//! Weight given to each new rate observation. Low, because the estimate only
//! needs to move over minutes.
constexpr double RATE_EMA_ALPHA = 0.15;
}

double VideoSyncClock::secondsBetween(TimePoint from, TimePoint to)
{
    return std::chrono::duration_cast<std::chrono::duration<double> >(to - from).count();
}

double VideoSyncClock::grainForSampleRate(double sampleRate)
{
    const double rate = sampleRate > 0.0 ? sampleRate : FALLBACK_SAMPLE_RATE;
    return TIME_QUEUE_GRAIN_SAMPLES / rate;
}

void VideoSyncClock::setConfig(const Config& config)
{
    m_config = config;
}

double VideoSyncClock::deadband() const
{
    // Never chase the reports' own quantisation, and never chase an error too
    // small to change which frame is shown.
    return std::max(2.0 * m_config.grain, 0.5 * m_config.frameDuration);
}

void VideoSyncClock::hardAnchor(muse::secs_t reported, TimePoint wall)
{
    m_anchorProject = reported.to_double();
    m_anchorWall = wall;
    m_lastEmitted = m_anchorProject;
}

void VideoSyncClock::invalidateRateReference()
{
    m_rateRefValid = false;
}

void VideoSyncClock::start(muse::secs_t reported, TimePoint wall)
{
    m_advancing = true;
    hardAnchor(reported, wall);
    m_ratio = 1.0;
    m_lastError = 0.0;
    m_lastEventWall = wall;
    m_haveEvent = true;
    invalidateRateReference();
}

void VideoSyncClock::stop(muse::secs_t reported)
{
    m_advancing = false;
    m_anchorProject = reported.to_double();
    m_lastEmitted = m_anchorProject;
    m_lastError = 0.0;
    invalidateRateReference();
}

void VideoSyncClock::setLoopRegion(const LoopRegion& loop)
{
    m_loop = loop;
}

bool VideoSyncClock::looksLikeLoopWrap(double reported, double error) const
{
    if (!m_loop.usable()) {
        return false;
    }

    const double length = m_loop.length();

    // Longer loops wrap by more than the hard resync threshold, and that
    // branch already does exactly this. Claiming them here would only be a
    // second route to the same place.
    if (length >= m_config.hardResync) {
        return false;
    }

    // Backwards, and by more than the reports' own quantisation.
    if (error >= -deadband()) {
        return false;
    }

    // A wrap steps back by roughly the loop length: the estimate was near the
    // loop end and the report is near its start. Scaling the test to the loop
    // rather than using a fixed window is what stops this degenerating into
    // "any backwards jitter is a wrap" on exactly the short loops it exists
    // to serve.
    if (-error < 0.5 * length) {
        return false;
    }

    // And the report has to be inside the loop. One grain of slack below the
    // start, because the reported value is a quantised queue record.
    return reported >= m_loop.start - m_config.grain && reported <= m_loop.end;
}

VideoSyncClock::Response VideoSyncClock::onPosition(muse::secs_t reported, TimePoint wall)
{
    // Predict before recording this report, not after. Stamping the report
    // time first would make isStalled() false again immediately, so a clock
    // that had frozen through a gap would silently free-run across it and the
    // error against the returning report would look small enough to absorb.
    const double predicted = m_advancing ? position(wall).to_double() : 0.0;

    m_lastEventWall = wall;
    m_haveEvent = true;

    if (!m_advancing) {
        // Stopped or paused, so every report is a seek. The player sets its
        // position unconditionally on seek and the channel always sends, which
        // means the same value arrives repeatedly while a playhead is held
        // still; decoding those again would be pure waste.
        if (std::fabs(reported.to_double() - m_anchorProject) < 1e-9) {
            m_lastError = muse::secs_t(0.0);
            return Response::Continue;
        }

        m_anchorProject = reported.to_double();
        m_anchorWall = wall;
        m_lastEmitted = m_anchorProject;
        m_lastError = muse::secs_t(0.0);
        return Response::Reanchored;
    }

    const double error = reported.to_double() - predicted;
    m_lastError = muse::secs_t(error);

    if (std::fabs(error) > m_config.hardResync) {
        // A seek while playing, a long loop wrapping, or the audio stream
        // being torn down and rebuilt for a device change.
        hardAnchor(reported, wall);
        m_ratio = 1.0;
        invalidateRateReference();
        return Response::Reanchored;
    }

    if (looksLikeLoopWrap(reported.to_double(), error)) {
        // Short loops wrap by less than the threshold above, so without this
        // the correction falls into the slew branch, where the monotonic
        // guard clamps it forward and the estimate can never move back. The
        // picture would run past the loop end and saw back once the error
        // finally grew past the threshold, over and over.
        //
        // m_lastError keeps the real magnitude, so the published drift still
        // shows the wrap rather than hiding it.
        hardAnchor(reported, wall);
        m_ratio = 1.0;
        invalidateRateReference();
        return Response::Reanchored;
    }

    if (std::fabs(error) > deadband()) {
        // Partial correction, and never backwards: a picture that jumps back a
        // frame reads far worse than one briefly a frame behind.
        const double corrected = predicted + m_config.slewGain * error;
        m_anchorProject = std::max(m_lastEmitted, corrected);
        m_anchorWall = wall;
    }

    updateRateEstimate(reported, wall);
    return Response::Continue;
}

void VideoSyncClock::updateRateEstimate(muse::secs_t reported, TimePoint wall)
{
    if (!m_rateRefValid) {
        m_rateRefProject = reported.to_double();
        m_rateRefWall = wall;
        m_rateRefValid = true;
        return;
    }

    const double elapsedWall = secondsBetween(m_rateRefWall, wall);
    if (elapsedWall < m_config.rateWindow) {
        return;
    }

    const double elapsedProject = reported.to_double() - m_rateRefProject;
    if (elapsedWall > 0.0) {
        const double observed = std::clamp(elapsedProject / elapsedWall, MIN_RATIO, MAX_RATIO);
        m_ratio = (1.0 - RATE_EMA_ALPHA) * m_ratio + RATE_EMA_ALPHA * observed;
    }

    m_rateRefProject = reported.to_double();
    m_rateRefWall = wall;
}

muse::secs_t VideoSyncClock::position(TimePoint wall) const
{
    if (!m_advancing) {
        return muse::secs_t(m_anchorProject);
    }

    if (isStalled(wall)) {
        // The player's own position freezes when the audio callback stops
        // delivering, so freeze with it rather than free-running past it.
        return muse::secs_t(std::max(m_lastEmitted, m_anchorProject));
    }

    const double elapsed = secondsBetween(m_anchorWall, wall);
    const double estimate = m_anchorProject + m_ratio * elapsed;
    return muse::secs_t(std::max(m_lastEmitted, estimate));
}

muse::secs_t VideoSyncClock::advanceTo(TimePoint wall)
{
    const muse::secs_t value = position(wall);
    m_lastEmitted = value.to_double();
    return value;
}

bool VideoSyncClock::isStalled(TimePoint wall) const
{
    if (!m_advancing || !m_haveEvent) {
        return false;
    }
    return secondsBetween(m_lastEventWall, wall) > m_config.stallSeconds;
}
