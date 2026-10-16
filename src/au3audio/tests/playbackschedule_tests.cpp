/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "au3-audio-io/PlaybackSchedule.h"

namespace au::au3audio {
namespace {
constexpr double RATE = 48000.0;
constexpr double RECORD_SECONDS = TimeQueueGrainSize / RATE;

class PlaybackScheduleTimeQueueTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_schedule.mT0 = 0.0;
        m_schedule.mT1 = 100.0;
        m_schedule.mEnvelope = nullptr;
        m_schedule.GetPolicy().Initialize(m_schedule, RATE);
    }

    void produceRecord()
    {
        m_schedule.mTimeQueue.Producer(m_schedule, PlaybackSlice { TimeQueueGrainSize, TimeQueueGrainSize, TimeQueueGrainSize });
    }

    double consumeRecord()
    {
        return m_schedule.mTimeQueue.Consumer(TimeQueueGrainSize, RATE);
    }

    PlaybackSchedule m_schedule;
};
}

// The producer fills the ring up to the slot before the consumer's head after
// the tail has wrapped; it must grow the queue instead of overwriting unread times
TEST_F(PlaybackScheduleTimeQueueTests, ConsumerKeepsTimeWhenProducerFillsWrappedRing)
{
    m_schedule.mTimeQueue.Init(4);
    m_schedule.mTimeQueue.Prime(0.0);

    produceRecord();
    produceRecord();
    EXPECT_NEAR(consumeRecord(), 1 * RECORD_SECONDS, 1e-9);
    EXPECT_NEAR(consumeRecord(), 2 * RECORD_SECONDS, 1e-9);

    for (int i = 0; i < 4; ++i) {
        produceRecord();
    }

    for (int i = 3; i <= 6; ++i) {
        EXPECT_NEAR(consumeRecord(), i * RECORD_SECONDS, 1e-9) << "record " << i;
    }
}

// The producer writes the time records of a whole slice in one call; when the
// queue grows in the middle of a call, the records already written to the old
// node must still reach the consumer
TEST_F(PlaybackScheduleTimeQueueTests, ConsumerKeepsTimeWhenQueueGrowsWithinOneSlice)
{
    m_schedule.mTimeQueue.Init(4);
    m_schedule.mTimeQueue.Prime(0.0);

    constexpr size_t records = 6;
    constexpr size_t frames = records * TimeQueueGrainSize;
    m_schedule.mTimeQueue.Producer(m_schedule, PlaybackSlice { frames, frames, frames });

    for (size_t i = 1; i <= records; ++i) {
        EXPECT_NEAR(consumeRecord(), i * RECORD_SECONDS, 1e-9) << "record " << i;
    }
}
}
