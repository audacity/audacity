/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <map>
#include <vector>

#include "../internal/juce/captureinput.h"

namespace {
constexpr int TEST_FRAMES = 4;
constexpr float TEST_VOLUME = 0.5f;

//! Records what each channel of the main meter got
class TestMeter final : public au::audio::IAudioMeter
{
public:
    void push(uint8_t channel, const InterleavedSampleData& data, const std::optional<TrackId>& trackId) override
    {
        if (trackId) {
            return;
        }
        auto& samples = main[channel];
        samples.clear();
        for (size_t i = 0; i < data.frames; ++i) {
            samples.push_back(data.buffer[i * data.nChannels]);
        }
    }
    void start(double) override {}
    void stop() override {}
    muse::async::Channel<au::audio::audioch_t, au::audio::MeterSignal> dataChanged(const std::optional<TrackId>&) override
    {
        return {};
    }

    std::map<uint8_t, std::vector<float> > main;
};

std::vector<float> scaled(const std::vector<float>& samples, float factor)
{
    std::vector<float> result;
    for (const auto sample : samples) {
        result.push_back(sample * factor);
    }
    return result;
}
}

namespace au::au4juce {
TEST(CaptureInputTests, ReadsInterleavedWithTheVolumeApplied)
{
    const std::vector<float> left { 1.f, 2.f, 3.f, 4.f };
    const std::vector<float> right { -1.f, -2.f, -3.f, -4.f };
    const float* input[] { left.data(), right.data() };
    constexpr int numChannels = 2;
    CaptureInput capture { numChannels, TEST_FRAMES };

    const auto samples = capture.read(input, TEST_FRAMES, TEST_VOLUME);

    for (int i = 0; i < TEST_FRAMES; ++i) {
        EXPECT_FLOAT_EQ(samples[i * numChannels], left[i] * TEST_VOLUME);
        EXPECT_FLOAT_EQ(samples[i * numChannels + 1], right[i] * TEST_VOLUME);
    }
}

TEST(CaptureInputTests, MonoInputIsPlayedThroughToBothChannels)
{
    const std::vector<float> mono { 1.f, 2.f, 3.f, 4.f };
    const float* input[] { mono.data() };
    CaptureInput capture { 1, TEST_FRAMES };
    capture.read(input, TEST_FRAMES, TEST_VOLUME);

    constexpr int numOutputChannels = 2;
    std::vector<float> left(TEST_FRAMES), right(TEST_FRAMES);
    float* output[] { left.data(), right.data() };
    constexpr int offset = 0;
    capture.playThrough(output, numOutputChannels, offset);

    EXPECT_EQ(left, scaled(mono, TEST_VOLUME));
    EXPECT_EQ(right, scaled(mono, TEST_VOLUME));
}

TEST(CaptureInputTests, MainMeterShowsTheLoudestChannelsOfInputsOfMoreThanTwo)
{
    //! [GIVEN] four channels, the louder of each side being the third and the second
    const std::vector<float> first { 0.1f, 0.1f, 0.1f, 0.1f };
    const std::vector<float> second { -0.4f, -0.4f, -0.4f, -0.4f };
    const std::vector<float> third { 0.3f, 0.3f, 0.3f, 0.3f };
    const std::vector<float> fourth { 0.2f, 0.2f, 0.2f, 0.2f };
    const float* input[] { first.data(), second.data(), third.data(), fourth.data() };
    constexpr int numChannels = 4;
    CaptureInput capture { numChannels, TEST_FRAMES };
    constexpr float unitVolume = 1.f;
    capture.read(input, TEST_FRAMES, unitVolume);

    //! [WHEN] it is metered, while monitoring
    TestMeter meter;
    capture.sendToMeter(meter, {}, {});

    //! [THEN] each side of the main meter shows the louder of its channels
    EXPECT_EQ(meter.main[0], third);
    EXPECT_EQ(meter.main[1], second);
}
}
