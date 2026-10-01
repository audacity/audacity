/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include <chrono>
#include <cmath>

#include "internal/videosyncclock.h"

using namespace au::video;
using namespace std::chrono_literals;

namespace {
using TimePoint = VideoSyncClock::TimePoint;

//! A wall clock the test drives by hand, so none of this waits for real time.
class FakeWall
{
public:
    TimePoint now() const { return m_now; }

    TimePoint advance(double seconds)
    {
        m_now += std::chrono::duration_cast<TimePoint::duration>(
            std::chrono::duration<double>(seconds));
        return m_now;
    }

private:
    TimePoint m_now { TimePoint::duration(1'000'000'000) };
};

VideoSyncClock::Config config25fps()
{
    VideoSyncClock::Config c;
    c.grain = 480.0 / 48000.0;       // 10 ms
    c.frameDuration = 1.0 / 25.0;    // 40 ms
    return c;
}

//! Plays for a while, feeding position reports at the player's own cadence.
//! `drift` scales how fast reported time runs against the wall, which is how a
//! sound card that is not quite at nominal rate behaves.
double playFor(VideoSyncClock& clock, FakeWall& wall, double seconds,
               double startPosition = 0.0, double drift = 1.0,
               double reportInterval = 0.016)
{
    double reported = startPosition;
    for (double t = 0.0; t < seconds; t += reportInterval) {
        const TimePoint now = wall.advance(reportInterval);
        reported += reportInterval * drift;
        clock.onPosition(muse::secs_t(reported), now);
        clock.advanceTo(now);
    }
    return reported;
}
}

// ---------------------------------------------------------------------------
// Stopped
// ---------------------------------------------------------------------------

TEST(VideoSyncClockTests, HoldsStillWhenNotAdvancing)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;

    clock.stop(muse::secs_t(4.0));
    EXPECT_FALSE(clock.isAdvancing());
    EXPECT_DOUBLE_EQ(clock.position(wall.now()).to_double(), 4.0);

    // Time passing must not move a stopped playhead.
    EXPECT_DOUBLE_EQ(clock.position(wall.advance(10.0)).to_double(), 4.0);
}

TEST(VideoSyncClockTests, EverySeekWhileStoppedReanchors)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;
    clock.stop(muse::secs_t(0.0));

    EXPECT_EQ(clock.onPosition(muse::secs_t(3.0), wall.advance(0.1)),
              VideoSyncClock::Response::Reanchored);
    EXPECT_DOUBLE_EQ(clock.position(wall.now()).to_double(), 3.0);

    // Backwards too.
    EXPECT_EQ(clock.onPosition(muse::secs_t(1.0), wall.advance(0.1)),
              VideoSyncClock::Response::Reanchored);
    EXPECT_DOUBLE_EQ(clock.position(wall.now()).to_double(), 1.0);
}

TEST(VideoSyncClockTests, IgnoresRepeatsOfTheSameStoppedPosition)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;
    clock.stop(muse::secs_t(0.0));

    ASSERT_EQ(clock.onPosition(muse::secs_t(2.5), wall.advance(0.05)),
              VideoSyncClock::Response::Reanchored);

    // The player republishes its position on every seek even when the value
    // has not changed, and holding a playhead still produces a stream of them.
    for (int i = 0; i < 5; ++i) {
        EXPECT_EQ(clock.onPosition(muse::secs_t(2.5), wall.advance(0.05)),
                  VideoSyncClock::Response::Continue)
            << "repeat " << i << " should not ask for another decode";
    }
}

// ---------------------------------------------------------------------------
// Interpolation between reports
// ---------------------------------------------------------------------------

TEST(VideoSyncClockTests, InterpolatesBetweenPositionReports)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;

    clock.start(muse::secs_t(0.0), wall.now());
    EXPECT_TRUE(clock.isAdvancing());

    // No report yet, but the wall moved: the estimate must move with it,
    // otherwise the picture only updates at the player's report rate.
    EXPECT_NEAR(clock.position(wall.advance(0.008)).to_double(), 0.008, 1e-6);
    EXPECT_NEAR(clock.position(wall.advance(0.008)).to_double(), 0.016, 1e-6);
}

TEST(VideoSyncClockTests, TracksSteadyPlaybackClosely)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;

    clock.start(muse::secs_t(0.0), wall.now());
    // Accumulating a floating point interval does not land exactly on the
    // nominal total, so compare against where the reports actually got to.
    const double reached = playFor(clock, wall, 30.0);

    EXPECT_NEAR(clock.position(wall.now()).to_double(), reached, 0.005);
    EXPECT_LT(std::abs(clock.lastError().to_double()), 0.005);
}

TEST(VideoSyncClockTests, NeverRunsBackwardsWhileAdvancing)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;

    clock.start(muse::secs_t(0.0), wall.now());

    double previous = 0.0;
    for (int i = 0; i < 400; ++i) {
        const TimePoint now = wall.advance(0.016);

        // Reports that jitter either side of the truth, which is what an
        // audio callback plus a timer actually produces.
        const double jitter = (i % 3 == 0) ? -0.012 : ((i % 3 == 1) ? 0.011 : 0.0);
        clock.onPosition(muse::secs_t(i * 0.016 + jitter), now);

        const double value = clock.advanceTo(now).to_double();
        EXPECT_GE(value, previous) << "went backwards at report " << i;
        previous = value;
    }
}

// ---------------------------------------------------------------------------
// Corrections
// ---------------------------------------------------------------------------

TEST(VideoSyncClockTests, IgnoresErrorsInsideTheDeadband)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;

    clock.start(muse::secs_t(0.0), wall.now());
    const double before = clock.deadband();

    // Deadband is the larger of two grains and half a frame: 20 ms of
    // quantisation against 20 ms of half-frame here.
    EXPECT_NEAR(before, 0.02, 1e-9);

    const TimePoint now = wall.advance(0.1);
    // A report 5 ms off is inside the deadband and must not move the anchor.
    clock.onPosition(muse::secs_t(0.105), now);
    EXPECT_NEAR(clock.position(now).to_double(), 0.1, 1e-6);
}

TEST(VideoSyncClockTests, CorrectsGraduallyOutsideTheDeadband)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;

    clock.start(muse::secs_t(0.0), wall.now());
    const double reached = playFor(clock, wall, 1.0);

    const TimePoint now = wall.advance(0.016);
    const double predicted = clock.position(now).to_double();

    // 50 ms ahead of where the clock expected: outside the deadband, inside
    // the hard resync threshold, so it should be absorbed a fifth at a time.
    clock.onPosition(muse::secs_t(predicted + 0.05), now);

    const double corrected = clock.position(now).to_double();
    EXPECT_GT(corrected, predicted) << "should have moved toward the report";
    EXPECT_LT(corrected, predicted + 0.05) << "should not have jumped the whole way";
    EXPECT_NEAR(corrected, predicted + 0.2 * 0.05, 1e-6);
    EXPECT_GT(reached, 0.9);
}

TEST(VideoSyncClockTests, HardResyncOnALargeJump)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;

    clock.start(muse::secs_t(0.0), wall.now());
    playFor(clock, wall, 2.0);

    // A seek while playing lands far outside anything a correction should
    // absorb, and the decoder needs to be told to seek rather than roll on.
    const TimePoint now = wall.advance(0.016);
    EXPECT_EQ(clock.onPosition(muse::secs_t(60.0), now),
              VideoSyncClock::Response::Reanchored);
    EXPECT_NEAR(clock.position(now).to_double(), 60.0, 1e-6);
    EXPECT_DOUBLE_EQ(clock.rateRatio(), 1.0) << "rate estimate must be discarded";
}

TEST(VideoSyncClockTests, HardResyncBackwardsForALoopWrap)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;

    clock.start(muse::secs_t(10.0), wall.now());
    playFor(clock, wall, 1.0, 10.0);

    // Loop wrapping back to its start is a big backwards jump, and the
    // monotonic guard must not block it.
    const TimePoint now = wall.advance(0.016);
    EXPECT_EQ(clock.onPosition(muse::secs_t(10.0), now),
              VideoSyncClock::Response::Reanchored);
    EXPECT_NEAR(clock.position(now).to_double(), 10.0, 1e-6);
}

// ---------------------------------------------------------------------------
// Rate estimation. The sound card and the CPU clock are independent, and the
// difference only shows up over minutes.
// ---------------------------------------------------------------------------

TEST(VideoSyncClockTests, StartsAtUnityRate)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;
    clock.start(muse::secs_t(0.0), wall.now());
    EXPECT_DOUBLE_EQ(clock.rateRatio(), 1.0);
}

TEST(VideoSyncClockTests, LearnsAConsistentRateDifference)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;

    clock.start(muse::secs_t(0.0), wall.now());
    // Reported time running 0.2% fast against the wall, held for long enough
    // that several rate windows close.
    playFor(clock, wall, 120.0, 0.0, 1.002);

    EXPECT_GT(clock.rateRatio(), 1.0005) << "should have noticed the difference";
    EXPECT_LT(clock.rateRatio(), 1.0025) << "and should not have overshot it";
}

TEST(VideoSyncClockTests, RateEstimateIsClampedAgainstNonsense)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;

    clock.start(muse::secs_t(0.0), wall.now());
    // A wildly wrong apparent rate, which in practice means something else
    // went wrong; believing it would make the picture race away.
    playFor(clock, wall, 120.0, 0.0, 1.5);

    EXPECT_LE(clock.rateRatio(), 1.0051);
    EXPECT_GE(clock.rateRatio(), 0.9949);
}

TEST(VideoSyncClockTests, RateEstimateSurvivesSmallCorrections)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;

    clock.start(muse::secs_t(0.0), wall.now());
    playFor(clock, wall, 60.0, 0.0, 1.002);
    const double learned = clock.rateRatio();
    ASSERT_GT(learned, 1.0);

    // A correction inside the hard-resync threshold must not throw away what
    // has been learned; only a real discontinuity should.
    clock.onPosition(muse::secs_t(clock.position(wall.now()).to_double() + 0.05),
                     wall.advance(0.016));
    EXPECT_DOUBLE_EQ(clock.rateRatio(), learned);
}

// ---------------------------------------------------------------------------
// Stalls
// ---------------------------------------------------------------------------

TEST(VideoSyncClockTests, FreezesWhenReportsStop)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;

    clock.start(muse::secs_t(0.0), wall.now());
    playFor(clock, wall, 2.0);

    const double lastGood = clock.position(wall.now()).to_double();
    EXPECT_FALSE(clock.isStalled(wall.now()));

    // The audio callback stops delivering, so the player's own position
    // freezes. Free-running through that turns a stall into drift that never
    // recovers.
    const TimePoint later = wall.advance(1.0);
    EXPECT_TRUE(clock.isStalled(later));
    EXPECT_NEAR(clock.position(later).to_double(), lastGood, 0.05);
}

TEST(VideoSyncClockTests, ResumesAfterAStall)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;

    clock.start(muse::secs_t(0.0), wall.now());
    playFor(clock, wall, 1.0);

    const TimePoint stalled = wall.advance(1.0);
    ASSERT_TRUE(clock.isStalled(stalled));

    // Reports come back, at a position that reflects the gap.
    const TimePoint back = wall.advance(0.016);
    clock.onPosition(muse::secs_t(2.0), back);
    EXPECT_FALSE(clock.isStalled(back));
    EXPECT_NEAR(clock.position(back).to_double(), 2.0, 1e-6);
}

TEST(VideoSyncClockTests, IsNotStalledWhenStopped)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;

    clock.stop(muse::secs_t(5.0));
    EXPECT_FALSE(clock.isStalled(wall.advance(60.0)))
        << "a stopped transport is not a stalled one";
}

// ---------------------------------------------------------------------------
// Transport transitions
// ---------------------------------------------------------------------------

TEST(VideoSyncClockTests, StopHoldsWherePlaybackReached)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;

    clock.start(muse::secs_t(0.0), wall.now());
    playFor(clock, wall, 3.0);

    const double reached = clock.position(wall.now()).to_double();
    clock.stop(muse::secs_t(reached));

    EXPECT_FALSE(clock.isAdvancing());
    EXPECT_NEAR(clock.position(wall.advance(5.0)).to_double(), reached, 1e-9);
}

TEST(VideoSyncClockTests, RestartAfterStopResumesFromTheNewPosition)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;

    clock.start(muse::secs_t(0.0), wall.now());
    playFor(clock, wall, 1.0);
    clock.stop(muse::secs_t(1.0));

    wall.advance(30.0);   // a long pause
    clock.start(muse::secs_t(7.0), wall.now());

    EXPECT_NEAR(clock.position(wall.now()).to_double(), 7.0, 1e-6);

    // Reports resume at the player's own cadence. Advancing half a second
    // without any would be a stall, and freezing there is the wanted
    // behaviour rather than something to assert against.
    const double reached = playFor(clock, wall, 0.5, 7.0);
    EXPECT_NEAR(clock.position(wall.now()).to_double(), reached, 0.005);
    EXPECT_GT(reached, 7.4);
}

TEST(VideoSyncClockTests, DeadbandFollowsTheSlowerOfGrainAndFrameRate)
{
    VideoSyncClock clock;

    VideoSyncClock::Config slowFrames;
    slowFrames.grain = 480.0 / 48000.0;    // 10 ms
    slowFrames.frameDuration = 1.0;        // a one second frame, as VFR allows
    clock.setConfig(slowFrames);
    EXPECT_NEAR(clock.deadband(), 0.5, 1e-9) << "half a frame dominates";

    VideoSyncClock::Config coarseGrain;
    coarseGrain.grain = 480.0 / 8000.0;    // 60 ms at a low project rate
    coarseGrain.frameDuration = 1.0 / 50.0;
    clock.setConfig(coarseGrain);
    EXPECT_NEAR(clock.deadband(), 0.12, 1e-9) << "two grains dominate";
}

// ---------------------------------------------------------------------------
// Loops.
//
// A loop longer than the hard resync threshold wraps by more than that
// threshold, and the existing branch already handles it. A shorter one wraps
// by less, so the correction falls into the slew branch, where the monotonic
// guard clamps it forward and the estimate can never move back. That is the
// case these cover.
// ---------------------------------------------------------------------------

namespace {
VideoSyncClock::LoopRegion loop(double start, double end, bool active = true)
{
    VideoSyncClock::LoopRegion region;
    region.start = start;
    region.end = end;
    region.active = active;
    return region;
}

//! Plays from the loop start to near its end, then returns the clock's
//! estimate just before the wrap report arrives.
double playToLoopEnd(VideoSyncClock& clock, FakeWall& wall, double start, double end)
{
    clock.start(muse::secs_t(start), wall.now());
    playFor(clock, wall, (end - start) * 0.9, start);
    return clock.position(wall.now()).to_double();
}
}

TEST(VideoSyncClockTests, ShortLoopWrapReanchors)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    clock.setLoopRegion(loop(10.0, 10.1));
    FakeWall wall;

    playToLoopEnd(clock, wall, 10.0, 10.1);

    const TimePoint now = wall.advance(0.016);
    EXPECT_EQ(clock.onPosition(muse::secs_t(10.006), now),
              VideoSyncClock::Response::Reanchored);
    EXPECT_NEAR(clock.position(now).to_double(), 10.006, 1e-6);
}

TEST(VideoSyncClockTests, ShortLoopWrapBeatsTheMonotonicGuard)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    clock.setLoopRegion(loop(10.0, 10.1));
    FakeWall wall;

    const double before = playToLoopEnd(clock, wall, 10.0, 10.1);

    const TimePoint now = wall.advance(0.016);
    clock.onPosition(muse::secs_t(10.006), now);

    // The whole point: the estimate has to be allowed back to the loop start.
    EXPECT_LT(clock.position(now).to_double(), before);
}

TEST(VideoSyncClockTests, WithoutALoopRegionTheSameJumpIsSwallowed)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;

    // Identical sequence with no loop set. This pins the behaviour the loop
    // branch exists to fix: the jump is too small for the hard resync, so the
    // monotonic guard holds the estimate forward.
    const double before = playToLoopEnd(clock, wall, 10.0, 10.1);

    const TimePoint now = wall.advance(0.016);
    EXPECT_EQ(clock.onPosition(muse::secs_t(10.006), now),
              VideoSyncClock::Response::Continue);
    EXPECT_GE(clock.position(now).to_double(), before);
}

TEST(VideoSyncClockTests, InactiveLoopRegionIsIgnored)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    clock.setLoopRegion(loop(10.0, 10.1, false));
    FakeWall wall;

    playToLoopEnd(clock, wall, 10.0, 10.1);

    const TimePoint now = wall.advance(0.016);
    EXPECT_EQ(clock.onPosition(muse::secs_t(10.006), now),
              VideoSyncClock::Response::Continue);
}

TEST(VideoSyncClockTests, LongLoopIsLeftToTheHardResync)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    clock.setLoopRegion(loop(10.0, 15.0));
    FakeWall wall;

    clock.start(muse::secs_t(10.0), wall.now());
    playFor(clock, wall, 1.0, 10.0);

    // Small backwards jitter inside a long loop is jitter, not a wrap; a real
    // wrap here would exceed the hard resync threshold and take that branch.
    const TimePoint now = wall.advance(0.016);
    const double predicted = clock.position(now).to_double();
    EXPECT_EQ(clock.onPosition(muse::secs_t(predicted - 0.030), now),
              VideoSyncClock::Response::Continue);
}

TEST(VideoSyncClockTests, NegativeJitterInsideAShortLoopIsNotAWrap)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    clock.setLoopRegion(loop(10.0, 10.1));
    FakeWall wall;

    clock.start(muse::secs_t(10.0), wall.now());
    playFor(clock, wall, 0.05, 10.0);

    // 25 ms back is outside the deadband but well under half the loop, so it
    // is jitter. Without the length-relative test this would be a false wrap.
    const TimePoint now = wall.advance(0.016);
    const double predicted = clock.position(now).to_double();
    EXPECT_EQ(clock.onPosition(muse::secs_t(predicted - 0.025), now),
              VideoSyncClock::Response::Continue);
}

TEST(VideoSyncClockTests, JitterInsideTheDeadbandIsNeverAWrap)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    clock.setLoopRegion(loop(10.0, 10.1));
    FakeWall wall;

    clock.start(muse::secs_t(10.0), wall.now());
    playFor(clock, wall, 0.05, 10.0);

    const TimePoint now = wall.advance(0.016);
    const double predicted = clock.position(now).to_double();
    EXPECT_EQ(clock.onPosition(muse::secs_t(predicted - 0.015), now),
              VideoSyncClock::Response::Continue);
}

TEST(VideoSyncClockTests, ReportOutsideTheLoopIsNotAWrap)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    // A shorter loop, so a jump that lands outside it is still small enough
    // to reach this branch rather than being claimed by the hard resync.
    clock.setLoopRegion(loop(10.0, 10.06));
    FakeWall wall;

    playToLoopEnd(clock, wall, 10.0, 10.06);

    // Landing before the loop start is a seek out of the loop, not a wrap.
    const TimePoint now = wall.advance(0.016);
    const double predicted = clock.position(now).to_double();
    ASSERT_LT(std::abs(9.97 - predicted), 0.15) << "must not trip the hard resync";

    EXPECT_EQ(clock.onPosition(muse::secs_t(9.97), now),
              VideoSyncClock::Response::Continue);
}

TEST(VideoSyncClockTests, ReversedLoopBoundsAreIgnored)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    // A real intermediate state while dragging a loop bar right to left.
    clock.setLoopRegion(loop(10.1, 10.0));
    FakeWall wall;

    clock.start(muse::secs_t(10.0), wall.now());
    playFor(clock, wall, 0.09, 10.0);

    const TimePoint now = wall.advance(0.016);
    EXPECT_EQ(clock.onPosition(muse::secs_t(10.006), now),
              VideoSyncClock::Response::Continue);
}

TEST(VideoSyncClockTests, ClearedLoopRegionIsIgnored)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    clock.setLoopRegion(loop(0.0, 0.0));
    FakeWall wall;

    clock.start(muse::secs_t(10.0), wall.now());
    playFor(clock, wall, 0.09, 10.0);

    const TimePoint now = wall.advance(0.016);
    EXPECT_EQ(clock.onPosition(muse::secs_t(10.006), now),
              VideoSyncClock::Response::Continue);
}

TEST(VideoSyncClockTests, WrapDiscardsTheRateEstimate)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;

    // Learn a rate first, over a long stretch with no loop in the way.
    clock.start(muse::secs_t(0.0), wall.now());
    playFor(clock, wall, 60.0, 0.0, 1.002);
    ASSERT_GT(clock.rateRatio(), 1.0);

    clock.setLoopRegion(loop(10.0, 10.1));
    clock.start(muse::secs_t(10.0), wall.now());
    playFor(clock, wall, 0.09, 10.0);

    const TimePoint now = wall.advance(0.016);
    ASSERT_EQ(clock.onPosition(muse::secs_t(10.006), now),
              VideoSyncClock::Response::Reanchored);
    EXPECT_DOUBLE_EQ(clock.rateRatio(), 1.0);
}

TEST(VideoSyncClockTests, WrapStillReportsItsMagnitude)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    clock.setLoopRegion(loop(10.0, 10.1));
    FakeWall wall;

    const double before = playToLoopEnd(clock, wall, 10.0, 10.1);

    const TimePoint now = wall.advance(0.016);
    const double predicted = clock.position(now).to_double();
    ASSERT_EQ(clock.onPosition(muse::secs_t(10.006), now),
              VideoSyncClock::Response::Reanchored);

    // The published drift has to keep showing the wrap rather than hiding it.
    EXPECT_NEAR(clock.lastError().to_double(), 10.006 - predicted, 1e-6);
    EXPECT_LT(clock.lastError().to_double(), 0.0);
    EXPECT_GT(before, 10.0);
}

TEST(VideoSyncClockTests, LoopRegionCanBeClearedAtRuntime)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    clock.setLoopRegion(loop(10.0, 10.1));
    FakeWall wall;

    playToLoopEnd(clock, wall, 10.0, 10.1);
    ASSERT_EQ(clock.onPosition(muse::secs_t(10.006), wall.advance(0.016)),
              VideoSyncClock::Response::Reanchored);

    // Turning looping off has to stop the detection immediately.
    clock.setLoopRegion(VideoSyncClock::LoopRegion());
    EXPECT_FALSE(clock.loopRegion().usable());

    playFor(clock, wall, 0.09, 10.006);
    EXPECT_EQ(clock.onPosition(muse::secs_t(10.006), wall.advance(0.016)),
              VideoSyncClock::Response::Continue);
}

// ---------------------------------------------------------------------------
// Report quantisation.
//
// The time queue consumes a fixed number of samples per record, so how coarse
// the player's position reports are depends on the rate the stream is actually
// running at - the negotiated device rate, not the project's nominal one.
// Assuming 44.1 kHz on a 48 kHz stream overstates it by about 9%.
// ---------------------------------------------------------------------------

TEST(VideoSyncClockTests, GrainFollowsTheSampleRate)
{
    EXPECT_NEAR(VideoSyncClock::grainForSampleRate(44100.0), 480.0 / 44100.0, 1e-12);
    EXPECT_NEAR(VideoSyncClock::grainForSampleRate(48000.0), 480.0 / 48000.0, 1e-12);
    EXPECT_NEAR(VideoSyncClock::grainForSampleRate(96000.0), 480.0 / 96000.0, 1e-12);

    // 10.9 ms against 10.0 ms.
    EXPECT_GT(VideoSyncClock::grainForSampleRate(44100.0),
              VideoSyncClock::grainForSampleRate(48000.0));
}

TEST(VideoSyncClockTests, GrainFallsBackWhenNoStreamHasOpened)
{
    // getPlaybackSampleRate() reads zero until a stream has been negotiated.
    EXPECT_NEAR(VideoSyncClock::grainForSampleRate(0.0),
                VideoSyncClock::grainForSampleRate(VideoSyncClock::FALLBACK_SAMPLE_RATE),
                1e-12);
    EXPECT_NEAR(VideoSyncClock::grainForSampleRate(-1.0),
                VideoSyncClock::grainForSampleRate(VideoSyncClock::FALLBACK_SAMPLE_RATE),
                1e-12);
}

TEST(VideoSyncClockTests, TheSampleRateChangesTheDeadbandAtHighFrameRates)
{
    VideoSyncClock clock;

    // Below about 44 fps the half-frame term dominates and the grain is
    // invisible, which is why the wrong constant went unnoticed. At 120 fps
    // half a frame is 4.2 ms and two grains is 20-22 ms, so the grain decides.
    VideoSyncClock::Config at441;
    at441.grain = VideoSyncClock::grainForSampleRate(44100.0);
    at441.frameDuration = 1.0 / 120.0;
    clock.setConfig(at441);
    const double deadband441 = clock.deadband();

    VideoSyncClock::Config at48;
    at48.grain = VideoSyncClock::grainForSampleRate(48000.0);
    at48.frameDuration = 1.0 / 120.0;
    clock.setConfig(at48);
    const double deadband48 = clock.deadband();

    EXPECT_NEAR(deadband441, 2.0 * 480.0 / 44100.0, 1e-12);
    EXPECT_NEAR(deadband48, 2.0 * 480.0 / 48000.0, 1e-12);
    EXPECT_GT(deadband441, deadband48)
        << "assuming 44.1 kHz on a 48 kHz stream makes the deadband too wide";

    // The size of the error the wrong constant was producing.
    EXPECT_NEAR(deadband441 / deadband48, 48000.0 / 44100.0, 1e-9);
}

TEST(VideoSyncClockTests, TheSampleRateIsInvisibleAtOrdinaryFrameRates)
{
    VideoSyncClock clock;

    // 25 fps: half a frame is 20 ms, two grains is 20-22 ms. They are close
    // enough that the frame term wins at 48 kHz, which is exactly why this
    // was harmless in practice and worth pinning rather than assuming.
    VideoSyncClock::Config at48;
    at48.grain = VideoSyncClock::grainForSampleRate(48000.0);
    at48.frameDuration = 1.0 / 25.0;
    clock.setConfig(at48);

    EXPECT_NEAR(clock.deadband(), 0.5 / 25.0, 1e-12)
        << "at 25 fps the half-frame term should still dominate";
}
