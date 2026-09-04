/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_VIDEO_VIDEOSYNCCLOCK_H
#define AU_VIDEO_VIDEOSYNCCLOCK_H

#include <chrono>

#include "global/types/secs.h"

namespace au::video {
//! Estimates where the playhead is between position reports.
//!
//! The player publishes its position from a timer that ticks about every 16 ms,
//! and each value is derived from the audio callback rather than from a free
//! running clock. That is a good master to follow, but a panel that only
//! repainted on those reports would inherit their jitter, and one that ignored
//! them would drift. So this interpolates between reports with the wall clock
//! and re-anchors on every report.
//!
//! Three things it deliberately does:
//!
//! It keeps a rate ratio between the sound card and the CPU clock, rather than
//! resetting to unity on every correction. The two are independent oscillators
//! and differ by a few parts in ten thousand, which is invisible per tick and
//! accumulates over a long session.
//!
//! It never runs backwards while advancing. A partial correction that would
//! move the estimate backwards is clamped, because a picture that jumps back a
//! frame reads as a much worse fault than one that is briefly a frame behind.
//!
//! It freezes when position reports stop arriving. If the audio callback
//! stalls, the player's own position freezes; free-running through that would
//! turn a stall into visible drift that never recovers.
//!
//! All wall-clock readings are passed in rather than taken internally, so the
//! behaviour can be tested without waiting for real time to pass.
class VideoSyncClock
{
public:
    using TimePoint = std::chrono::steady_clock::time_point;

    struct Config {
        //! Quantisation of the player's position reports, in seconds. It is
        //! the playback time queue's grain: 480 samples at the project rate,
        //! so 10.9 ms at 44.1 kHz rather than a round 10 ms.
        double grain = 480.0 / 44100.0;

        //! Duration of one video frame, used to size the deadband. There is no
        //! point chasing an error smaller than half a frame.
        double frameDuration = 1.0 / 25.0;

        //! Above this error the estimate is thrown away and re-anchored. A
        //! seek, a loop wrap or an audio device change all land here.
        double hardResync = 0.150;

        //! Fraction of a small error corrected per report. Full correction
        //! would track the reports' own jitter.
        double slewGain = 0.20;

        //! No position report for this long means the audio callback stalled.
        double stallSeconds = 0.250;

        //! How much clean playback to observe before believing a rate estimate.
        double rateWindow = 5.0;
    };

    //! What the caller should do after a position report.
    enum class Response {
        Continue,     //!< keep interpolating; no decode needed beyond the usual
        Reanchored,   //!< the estimate jumped; the decoder needs a real seek
    };

    void setConfig(const Config& config);
    const Config& config() const { return m_config; }

    //! Transport started or resumed.
    void start(muse::secs_t reported, TimePoint wall);

    //! Transport paused or stopped. The estimate holds at the given position.
    void stop(muse::secs_t reported);

    bool isAdvancing() const { return m_advancing; }

    //! Feed a position report from the player.
    Response onPosition(muse::secs_t reported, TimePoint wall);

    //! Tell the clock a loop wrapped, so a large backwards jump is expected.
    void notifyReanchor(muse::secs_t reported, TimePoint wall);

    //! Current estimate. Monotonic while advancing.
    muse::secs_t position(TimePoint wall) const;

    //! Advances the monotonic guard and returns the estimate. Call this once
    //! per repaint; position() alone does not commit the value.
    muse::secs_t advanceTo(TimePoint wall);

    //! No position report for longer than the stall threshold.
    bool isStalled(TimePoint wall) const;

    //! Sound card against CPU clock. Converges toward the truth; starts at 1.
    double rateRatio() const { return m_ratio; }

    //! Difference between the last report and what was predicted for it. The
    //! panel publishes this so a sync complaint can be a number.
    muse::secs_t lastError() const { return m_lastError; }

    double deadband() const;

private:
    void hardAnchor(muse::secs_t reported, TimePoint wall);
    void invalidateRateReference();
    void updateRateEstimate(muse::secs_t reported, TimePoint wall);
    static double secondsBetween(TimePoint from, TimePoint to);

    Config m_config;

    bool m_advancing = false;
    double m_anchorProject = 0.0;
    TimePoint m_anchorWall {};
    double m_ratio = 1.0;
    double m_lastEmitted = 0.0;
    muse::secs_t m_lastError = 0.0;

    TimePoint m_lastEventWall {};
    bool m_haveEvent = false;

    double m_rateRefProject = 0.0;
    TimePoint m_rateRefWall {};
    bool m_rateRefValid = false;
};
}

#endif // AU_VIDEO_VIDEOSYNCCLOCK_H
