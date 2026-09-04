/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include <chrono>

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

TEST(VideoSyncClockTests, ExplicitReanchorResetsEverything)
{
    VideoSyncClock clock;
    clock.setConfig(config25fps());
    FakeWall wall;

    clock.start(muse::secs_t(0.0), wall.now());
    playFor(clock, wall, 20.0, 0.0, 1.002);

    clock.notifyReanchor(muse::secs_t(3.0), wall.now());
    EXPECT_NEAR(clock.position(wall.now()).to_double(), 3.0, 1e-6);
    EXPECT_DOUBLE_EQ(clock.rateRatio(), 1.0);
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
