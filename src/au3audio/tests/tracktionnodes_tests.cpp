/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <memory>
#include <vector>

#include "../internal/juce/stereo.h"
#include "../internal/tracktion/tracktiontoaudacitynode.h"

using namespace au::au4tracktion;

namespace {
constexpr double TEST_RATE = 44100.0;
constexpr size_t TEST_MAX_FRAMES = 64;
//! Longer than TEST_MAX_FRAMES, so that reads are split
constexpr size_t TEST_READ_LEN = 3 * TEST_MAX_FRAMES + TEST_MAX_FRAMES / 2;
constexpr int64_t TEST_START = 1'000;
constexpr float TEST_SCALE = 0.25f;

//! Its samples are their stream index, scaled
class RampNode final : public ISequenceProcessingNode
{
public:
    explicit RampNode(float scale)
        : m_scale{scale} {}

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override
    {
        for (int ch = 0; ch < au::au4juce::numChannels; ++ch) {
            for (size_t i = 0; i < len; ++i) {
                buffers[ch][i] = m_scale * static_cast<float>(start.as_long_long() + static_cast<int64_t>(i));
            }
        }
    }

private:
    const float m_scale;
};
}

TEST(TracktionToAudacityNodeTests, SumsItsTracksByStreamIndex)
{
    TracktionToAudacityNode node { { std::make_shared<RampNode>(1.f), std::make_shared<RampNode>(TEST_SCALE) }, TEST_RATE,
                                   TEST_MAX_FRAMES };
    std::vector<float> left(TEST_READ_LEN), right(TEST_READ_LEN);
    float* const buffers[] { left.data(), right.data() };

    node.GetSamples(buffers, TEST_START, TEST_READ_LEN);

    for (size_t i = 0; i < TEST_READ_LEN; ++i) {
        const auto expected = (1.f + TEST_SCALE) * static_cast<float>(TEST_START + static_cast<int64_t>(i));
        EXPECT_FLOAT_EQ(left[i], expected);
        EXPECT_FLOAT_EQ(right[i], expected);
    }
}
