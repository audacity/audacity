/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <cmath>
#include <numbers>
#include <vector>

#include "au3-clip-processing/sequence-graph/ResamplingNode.h"
#include "au3-preferences/Prefs.h"

#include "au3wrap/internal/au3commonsettings.h"

namespace {
constexpr double TEST_INPUT_RATE = 44100;
constexpr double TEST_OUTPUT_RATE = 48000;
constexpr double TEST_FREQUENCY = 440;
constexpr size_t TEST_NUM_CHANNELS = 1;
constexpr size_t TEST_LENGTH = 4800;
constexpr size_t TEST_BLOCK_SIZE = 333;
//! Of the resampled sine, compared with the sine at the output rate
constexpr float TEST_SINE_TOLERANCE = 1e-2f;

//! A sine starting at position 0
class SineNode final : public ISequenceProcessingNode
{
public:
    void GetSamples(float* const* buffers, sampleCount start, size_t len) override
    {
        for (size_t i = 0; i < len; ++i) {
            buffers[0][i] = sine((start.as_double() + i) / TEST_INPUT_RATE);
        }
    }

    static float sine(double time)
    {
        return static_cast<float>(std::sin(2 * std::numbers::pi * TEST_FREQUENCY * time));
    }
};

class ResamplingNodeTests : public ::testing::Test
{
protected:
    static void SetUpTestSuite()
    {
        // Resamplers read their method from the preferences
        if (!gPrefs) {
            InitPreferences(std::make_unique<au::au3::Au3CommonSettings>());
        }
    }
};

std::unique_ptr<ResamplingNode> makeNode()
{
    return std::make_unique<ResamplingNode>(std::make_unique<SineNode>(), TEST_NUM_CHANNELS, TEST_INPUT_RATE, TEST_OUTPUT_RATE);
}

std::vector<float> read(ResamplingNode& node, sampleCount start, size_t len)
{
    std::vector<float> buffer(len);
    float* const buffers[] { buffer.data() };
    node.GetSamples(buffers, start, len);
    return buffer;
}
}

TEST_F(ResamplingNodeTests, outputIsTheInputAtTheOutputRateWithoutDelay)
{
    const auto node = makeNode();
    const auto output = read(*node, 0, TEST_LENGTH);
    for (size_t i = 0; i < TEST_LENGTH; ++i) {
        EXPECT_NEAR(output[i], SineNode::sine(i / TEST_OUTPUT_RATE), TEST_SINE_TOLERANCE) << "at " << i;
    }
}

TEST_F(ResamplingNodeTests, readsInBlocksGiveWhatOneReadGives)
{
    const auto whole = read(*makeNode(), 0, TEST_LENGTH);

    const auto node = makeNode();
    std::vector<float> blocks;
    while (blocks.size() < TEST_LENGTH) {
        const auto len = std::min(TEST_BLOCK_SIZE, TEST_LENGTH - blocks.size());
        const auto block = read(*node, blocks.size(), len);
        blocks.insert(blocks.end(), block.begin(), block.end());
    }

    EXPECT_EQ(blocks, whole);
}

TEST_F(ResamplingNodeTests, aReadElsewhereRestartsThere)
{
    constexpr size_t start = TEST_LENGTH / 2;
    const auto node = makeNode();
    read(*node, 0, TEST_BLOCK_SIZE);
    const auto afterJump = read(*node, start, TEST_BLOCK_SIZE);
    EXPECT_EQ(afterJump, read(*makeNode(), start, TEST_BLOCK_SIZE));
}
