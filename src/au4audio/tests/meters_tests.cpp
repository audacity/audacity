/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <chrono>
#include <memory>
#include <vector>

#include "../internal/metersender.h"
#include "../internal/metertapnode.h"
#include "../internal/stereo.h"

using namespace au::au4audio;
using au::audio::IAudioMeter;
using au::audio::TimePoint;

namespace {
constexpr size_t TEST_BLOCK = 100;
constexpr size_t TEST_FIRST_PART = 40;
constexpr int64_t TEST_TRACK_ID = 7;
constexpr int64_t TEST_LEAD = 30;

//! Its samples are their stream index
class RampNode final : public ISequenceProcessingNode
{
public:
    void GetSamples(float* const* buffers, sampleCount start, size_t len) override
    {
        for (int ch = 0; ch < au::au4audio::numChannels; ++ch) {
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

void render(ISequenceProcessingNode& node, int64_t start, size_t len)
{
    std::vector<float> left(len), right(len);
    float* const buffers[] { left.data(), right.data() };
    node.GetSamples(buffers, start, len);
}

std::shared_ptr<MeterTapNode> monoTap()
{
    return std::make_shared<MeterTapNode>(std::make_shared<RampNode>(), 1, TEST_BLOCK);
}

MeterSender trackSender(std::shared_ptr<MeterTapNode> tap, int64_t lead = 0)
{
    return MeterSender { { { std::move(tap), IAudioMeter::TrackId { TEST_TRACK_ID }, lead } } };
}
}

TEST(MeterSenderTests, SendsWhatATapKeptWhenItIsHeard)
{
    const auto tap = monoTap();
    const auto sender = trackSender(tap);
    render(*tap, 0, TEST_BLOCK);
    TestMeter meter;
    const auto time = TimePoint {} + std::chrono::seconds { 1 };

    sender.send(meter, 0, TEST_BLOCK, time);

    ASSERT_EQ(meter.pushes.size(), 1u);
    EXPECT_EQ(meter.pushes[0].samples, ramp(0, TEST_BLOCK));
    EXPECT_EQ(meter.pushes[0].time, time);
    ASSERT_TRUE(meter.pushes[0].trackId.has_value());
    EXPECT_EQ(meter.pushes[0].trackId->value, TEST_TRACK_ID);
}

TEST(MeterSenderTests, SendsEachChannelOfAStereoTap)
{
    const auto tap = std::make_shared<MeterTapNode>(std::make_shared<RampNode>(), au::au4audio::numChannels, TEST_BLOCK);
    const MeterSender sender { { { tap, std::nullopt, 0 } } };
    render(*tap, 0, TEST_BLOCK);
    TestMeter meter;

    sender.send(meter, 0, TEST_BLOCK, {});

    ASSERT_EQ(meter.pushes.size(), static_cast<size_t>(au::au4audio::numChannels));
    for (size_t ch = 0; ch < meter.pushes.size(); ++ch) {
        EXPECT_EQ(meter.pushes[ch].channel, ch);
        EXPECT_FALSE(meter.pushes[ch].trackId.has_value());
    }
}

TEST(MeterSenderTests, SendsWhatATapKeptAheadAsHeardAtTheStreamIndex)
{
    const auto tap = monoTap();
    const auto sender = trackSender(tap, TEST_LEAD);
    LeadingNode master { tap, TEST_LEAD };
    render(master, 0, TEST_BLOCK);
    TestMeter meter;

    sender.send(meter, 0, TEST_BLOCK, {});

    ASSERT_EQ(meter.pushes.size(), 1u);
    EXPECT_EQ(meter.pushes[0].samples, ramp(TEST_LEAD, TEST_BLOCK));
}

TEST(MeterSenderTests, DoesNotShowWhatATapDidNotKeepAllOf)
{
    const auto tap = monoTap();
    const auto sender = trackSender(tap);
    render(*tap, 0, TEST_FIRST_PART);
    TestMeter meter;

    sender.send(meter, 0, TEST_BLOCK, {});

    EXPECT_TRUE(meter.pushes.empty());
}

TEST(MeterSenderTests, ResetsTheTapsOnceSent)
{
    const auto tap = monoTap();
    const auto sender = trackSender(tap);
    render(*tap, 0, TEST_BLOCK);
    TestMeter meter;

    sender.send(meter, 0, TEST_BLOCK, {});
    sender.send(meter, 0, TEST_BLOCK, {});

    EXPECT_EQ(meter.pushes.size(), 1u);
}
