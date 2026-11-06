/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <chrono>
#include <memory>
#include <vector>

#include "../internal/juce/metersendnode.h"
#include "../internal/juce/metersink.h"
#include "../internal/juce/metertapnode.h"
#include "../internal/juce/stereo.h"

using namespace au::au4juce;
using au::audio::IAudioMeter;
using au::audio::TimePoint;

namespace {
constexpr size_t TEST_BLOCK = 100;
constexpr size_t TEST_FIRST_PART = 40;
constexpr double TEST_RATE = 1000.0;
constexpr int64_t TEST_TRACK_ID = 7;
constexpr int64_t TEST_LEAD = 30;
constexpr size_t TEST_NUM_BLOCKS = 4;

//! Its samples are their stream index
class RampNode final : public ISequenceProcessingNode
{
public:
    void GetSamples(float* const* buffers, sampleCount start, size_t len) override
    {
        for (int ch = 0; ch < au::au4juce::numChannels; ++ch) {
            for (size_t i = 0; i < len; ++i) {
                buffers[ch][i] = static_cast<float>(start.as_long_long() + static_cast<int64_t>(i));
            }
        }
    }
};

//! Reads its input ahead by `lead`, as the master's effects read the tracks
class LeadingNode final : public ISequenceProcessingNode
{
public:
    LeadingNode(std::shared_ptr<ISequenceProcessingNode> input, int64_t lead)
        : m_input{std::move(input)}, m_lead{lead} {}

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override
    {
        m_input->GetSamples(buffers, start + m_lead, len);
    }

private:
    const std::shared_ptr<ISequenceProcessingNode> m_input;
    const int64_t m_lead;
};

class TestMeter final : public IAudioMeter
{
public:
    struct Push {
        uint8_t channel = 0;
        std::vector<float> samples;
        TimePoint time;
        std::optional<TrackId> trackId;
    };

    void push(uint8_t channel, const InterleavedSampleData& data, const std::optional<TrackId>& trackId) override
    {
        Push push { channel, {}, data.dacTime, trackId };
        for (size_t i = 0; i < data.frames; ++i) {
            push.samples.push_back(data.buffer[i * data.nChannels]);
        }
        pushes.push_back(std::move(push));
    }
    void start(double) override {}
    void stop() override {}
    muse::async::Channel<au::audio::audioch_t, au::audio::MeterSignal> dataChanged(const std::optional<TrackId>&) override
    {
        return {};
    }

    std::vector<Push> pushes;
};

std::vector<float> ramp(int64_t start, size_t len)
{
    std::vector<float> values(len);
    for (size_t i = 0; i < len; ++i) {
        values[i] = static_cast<float>(start + static_cast<int64_t>(i));
    }
    return values;
}

MeterSink monoSink()
{
    return MeterSink { { { IAudioMeter::TrackId { TEST_TRACK_ID }, 1 } }, TEST_BLOCK, TEST_NUM_BLOCKS };
}

void writeRamp(MeterSink& sink, uint32_t generation, int64_t start)
{
    const auto samples = ramp(start, TEST_BLOCK);
    const float* const channels[] { samples.data() };
    sink.write(generation, start, TEST_BLOCK, channels);
}
}

TEST(MeterSinkTests, ForwardsWhatPlaysAsItPlays)
{
    auto sink = monoSink();
    writeRamp(sink, sink.generation(), 0);
    TestMeter meter;
    const TimePoint time {};

    sink.forward(meter, 0, TEST_FIRST_PART, time, TEST_RATE);
    sink.forward(meter, TEST_FIRST_PART, TEST_BLOCK - TEST_FIRST_PART, time, TEST_RATE);
    sink.forward(meter, TEST_BLOCK, TEST_BLOCK, time, TEST_RATE);

    ASSERT_EQ(meter.pushes.size(), 2u);
    EXPECT_EQ(meter.pushes[0].samples, ramp(0, TEST_FIRST_PART));
    EXPECT_EQ(meter.pushes[1].samples, ramp(TEST_FIRST_PART, TEST_BLOCK - TEST_FIRST_PART));
    ASSERT_TRUE(meter.pushes[0].trackId.has_value());
    EXPECT_EQ(meter.pushes[0].trackId->value, TEST_TRACK_ID);
}

TEST(MeterSinkTests, StampsWhatPlaysWithWhenItIsHeard)
{
    auto sink = monoSink();
    writeRamp(sink, sink.generation(), 0);
    TestMeter meter;
    const TimePoint time {};

    // From before the block: what it has is heard later in the callback
    constexpr int64_t before = 10;
    sink.forward(meter, -before, TEST_FIRST_PART, time, TEST_RATE);

    ASSERT_EQ(meter.pushes.size(), 1u);
    EXPECT_EQ(meter.pushes[0].time - time, std::chrono::duration_cast<TimePoint::duration>(std::chrono::duration<double>(before / TEST_RATE)));
}

TEST(MeterSinkTests, NeverShowsWhatWasRenderedBeforeInvalidating)
{
    auto sink = monoSink();
    const auto generation = sink.generation();
    sink.invalidate();
    writeRamp(sink, generation, 0);
    TestMeter meter;

    sink.forward(meter, 0, TEST_BLOCK, {}, TEST_RATE);

    EXPECT_TRUE(meter.pushes.empty());
}

TEST(MeterSinkTests, DropsWhatTheDevicePassed)
{
    auto sink = monoSink();
    writeRamp(sink, sink.generation(), 0);
    writeRamp(sink, sink.generation(), TEST_BLOCK);
    TestMeter meter;

    sink.forward(meter, TEST_BLOCK + TEST_FIRST_PART, TEST_FIRST_PART, {}, TEST_RATE);

    ASSERT_EQ(meter.pushes.size(), 1u);
    EXPECT_EQ(meter.pushes[0].samples, ramp(TEST_BLOCK + TEST_FIRST_PART, TEST_FIRST_PART));
}

TEST(MeterSendNodeTests, SendsWhatATapKeptAheadAsHeardAtTheStreamIndex)
{
    auto sink = monoSink();
    const auto tap = std::make_shared<MeterTapNode>(std::make_shared<RampNode>(), 1, TEST_BLOCK);
    MeterSendNode node { std::make_shared<LeadingNode>(tap, TEST_LEAD), { { tap, TEST_LEAD } }, sink };
    std::vector<float> left(TEST_BLOCK), right(TEST_BLOCK);
    float* const buffers[] { left.data(), right.data() };

    node.GetSamples(buffers, 0, TEST_BLOCK);

    TestMeter meter;
    sink.forward(meter, 0, TEST_BLOCK, {}, TEST_RATE);
    ASSERT_EQ(meter.pushes.size(), 1u);
    EXPECT_EQ(meter.pushes[0].samples, ramp(TEST_LEAD, TEST_BLOCK));
}
