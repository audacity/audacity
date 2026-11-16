/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "record/timedrecordingtypes.h"

namespace au::record {
namespace {
using namespace std::chrono_literals;
using TimePoint = TimedRecordingSchedule::TimePoint;

TimePoint at(std::chrono::hours hour, std::chrono::minutes minute = 0min)
{
    return TimePoint { std::chrono::hours { 24 * 20000 } } + hour + minute;
}

//! NOTE: gtest can't print chrono types on older macOS targets, so they're compared as plain numbers
int64_t seconds(TimePoint timePoint)
{
    return std::chrono::duration_cast<std::chrono::seconds>(timePoint.time_since_epoch()).count();
}

int64_t seconds(TimedRecordingSchedule::Duration duration)
{
    return duration.count();
}
}

TEST(TimedRecordingScheduleTests, StartingNowForADuration)
{
    TimedRecordingSchedule schedule;
    schedule.end = 1h;

    const TimePoint now = at(10h);
    EXPECT_EQ(seconds(schedule.startTime(now)), seconds(now));
    EXPECT_EQ(seconds(schedule.endTime(now)), seconds(at(11h)));
    EXPECT_EQ(seconds(schedule.recordingDuration(now)), seconds(1h));
    EXPECT_TRUE(schedule.isValid(now));
}

TEST(TimedRecordingScheduleTests, StartingAtATimeForADuration)
{
    TimedRecordingSchedule schedule;
    schedule.start = at(33h);
    schedule.end = 2h;

    const TimePoint now = at(10h);
    EXPECT_EQ(seconds(schedule.startTime(now)), seconds(at(33h)));
    EXPECT_EQ(seconds(schedule.endTime(now)), seconds(at(35h)));
    EXPECT_EQ(seconds(schedule.recordingDuration(now)), seconds(2h));
}

TEST(TimedRecordingScheduleTests, StartInThePastStartsNow)
{
    TimedRecordingSchedule schedule;
    schedule.start = at(9h);
    schedule.end = at(12h);

    const TimePoint now = at(10h);
    EXPECT_EQ(seconds(schedule.startTime(now)), seconds(now));
    EXPECT_EQ(seconds(schedule.recordingDuration(now)), seconds(2h));
}

TEST(TimedRecordingScheduleTests, NothingToRecordIsInvalid)
{
    const TimePoint now = at(10h);

    TimedRecordingSchedule noDuration;
    EXPECT_FALSE(noDuration.isValid(now));

    TimedRecordingSchedule endBeforeStart;
    endBeforeStart.start = at(12h);
    endBeforeStart.end = at(11h);
    EXPECT_FALSE(endBeforeStart.isValid(now));

    TimedRecordingSchedule endPassed;
    endPassed.end = at(9h);
    EXPECT_FALSE(endPassed.isValid(now));
}
}
