/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <limits>

#include "au3-audio-io/PlaybackSchedule.h"

//! Math checks for RecordingSchedule, in particular the re-baseline used when
//! capture is armed on a running playback stream (deferred capture): with
//! mLeadInTime = 0, the stream's negative latency compensation and an unbounded
//! duration, the existing ToDiscard()/Consumed() machinery behaves exactly like
//! a fresh recording started at the punch time.

namespace {
RecordingSchedule makeArmedSchedule(double latencyCompensation)
{
    RecordingSchedule schedule {};
    schedule.mLatencyCompensation = latencyCompensation;
    schedule.mDuration = std::numeric_limits<double>::max();
    return schedule;
}
}

/**
 * @brief A fresh recording discards the latency-compensation prefix
 * @details mPosition counts consumed ring-buffer seconds; the first |L| seconds
 *          are discarded, only then does Consumed() start growing
 */
TEST(RecordingScheduleTests, FreshRecording_DiscardsLatencyPrefix)
{
    RecordingSchedule schedule {};
    schedule.mLatencyCompensation = -0.25;
    schedule.mDuration = 10.0;

    //! [THEN] Before any input: the whole prefix is still to be discarded
    EXPECT_DOUBLE_EQ(schedule.TotalCorrection(), -0.25);
    EXPECT_DOUBLE_EQ(schedule.ToDiscard(), 0.25);
    EXPECT_DOUBLE_EQ(schedule.Consumed(), 0.0);
    EXPECT_DOUBLE_EQ(schedule.ToConsume(), 10.0);

    //! [WHEN] Exactly the prefix has been consumed
    schedule.mPosition = 0.25;

    //! [THEN] Nothing left to discard, nothing recorded yet
    EXPECT_DOUBLE_EQ(schedule.ToDiscard(), 0.0);
    EXPECT_DOUBLE_EQ(schedule.Consumed(), 0.0);

    //! [WHEN] One more second has been consumed
    schedule.mPosition = 1.25;

    //! [THEN] One second was recorded
    EXPECT_DOUBLE_EQ(schedule.Consumed(), 1.0);
    EXPECT_DOUBLE_EQ(schedule.ToConsume(), 9.0);
}

/**
 * @brief Lead-in recording discards the lead-in plus the latency prefix
 */
TEST(RecordingScheduleTests, LeadInRecording_DiscardsLeadInPlusLatency)
{
    RecordingSchedule schedule {};
    schedule.mLeadInTime = 2.0;
    schedule.mLatencyCompensation = -0.25;
    schedule.mDuration = 5.0;

    EXPECT_DOUBLE_EQ(schedule.TotalCorrection(), -2.25);
    EXPECT_DOUBLE_EQ(schedule.ToDiscard(), 2.25);

    schedule.mPosition = 2.25;
    EXPECT_DOUBLE_EQ(schedule.ToDiscard(), 0.0);
    EXPECT_DOUBLE_EQ(schedule.Consumed(), 0.0);

    schedule.mPosition = 3.25;
    EXPECT_DOUBLE_EQ(schedule.Consumed(), 1.0);
    EXPECT_DOUBLE_EQ(schedule.ToConsume(), 4.0);
}

/**
 * @brief The arm-time re-baseline is equivalent to a fresh recording
 * @details Deferred capture arms with lead-in 0 and the stream's latency
 *          compensation; discard/consumed behaviour must match a fresh
 *          recording started at the punch time, at every position
 */
TEST(RecordingScheduleTests, ArmedRebaseline_MatchesFreshRecording)
{
    const double latency = -0.25;

    RecordingSchedule armed = makeArmedSchedule(latency);

    RecordingSchedule fresh {};
    fresh.mLatencyCompensation = latency;
    fresh.mDuration = 10.0;

    for (double position : { 0.0, 0.1, 0.25, 0.5, 2.0, 9.0 }) {
        armed.mPosition = position;
        fresh.mPosition = position;

        EXPECT_DOUBLE_EQ(armed.ToDiscard(), fresh.ToDiscard()) << "position " << position;
        EXPECT_DOUBLE_EQ(armed.Consumed(), fresh.Consumed()) << "position " << position;
    }

    //! [THEN] The discarded prefix equals -TotalCorrection before the first append
    EXPECT_DOUBLE_EQ(armed.ToDiscard() + armed.mPosition - armed.Consumed(),
                     -armed.TotalCorrection());
}

/**
 * @brief The armed schedule never truncates the capture
 * @details The unbounded duration keeps ToConsume() effectively infinite, so
 *          DrainRecordBuffers never clips the batch to a remaining time
 */
TEST(RecordingScheduleTests, ArmedRebaseline_NeverTruncates)
{
    RecordingSchedule armed = makeArmedSchedule(-0.25);

    armed.mPosition = 3600.0; // an hour of capture
    EXPECT_GT(armed.ToConsume(), 1.0e9);
}

/**
 * @brief Zero latency compensation discards nothing
 */
TEST(RecordingScheduleTests, ArmedRebaseline_ZeroLatency_DiscardsNothing)
{
    RecordingSchedule armed = makeArmedSchedule(0.0);

    EXPECT_DOUBLE_EQ(armed.ToDiscard(), 0.0);

    armed.mPosition = 1.5;
    EXPECT_DOUBLE_EQ(armed.Consumed(), 1.5);
}
