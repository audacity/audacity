/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <memory>
#include <vector>

#include "au3-clip-processing/sequence-graph/IPlayheadMapper.h"

#include "../internal/endoftracknode.h"
#include "../internal/iendoftrackcontext.h"
#include "../internal/stereo.h"

using namespace au::au4audio;

namespace {
constexpr size_t TEST_MAX_FRAMES = 64;
//! Longer than TEST_MAX_FRAMES, so that reads are split
constexpr size_t TEST_READ_LEN = 3 * TEST_MAX_FRAMES + TEST_MAX_FRAMES / 2;
//! Where the playhead reads from index 0
constexpr int64_t TEST_PLAYHEAD_START = 1'000;
constexpr int64_t TEST_PLAY_END = TEST_PLAYHEAD_START + TEST_READ_LEN / 2;
constexpr int64_t TEST_TRACK_STOP = TEST_PLAYHEAD_START + TEST_READ_LEN / 4;

//! Its samples are their stream index, in the first `numChannels` buffers
class RampNode final : public ISequenceProcessingNode
{
public:
    explicit RampNode(size_t numChannels = au::au4audio::numChannels)
        : m_numChannels{numChannels} {}

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override
    {
        for (size_t ch = 0; ch < m_numChannels; ++ch) {
            for (size_t i = 0; i < len; ++i) {
                buffers[ch][i] = static_cast<float>(start.as_long_long() + static_cast<int64_t>(i));
            }
        }
    }

private:
    const size_t m_numChannels;
};

//! The playhead reads linearly from TEST_PLAYHEAD_START
class LinearMapper final : public IPlayheadMapper
{
public:
    Segment Map(sampleCount index, size_t maxLen) const override { return { index + TEST_PLAYHEAD_START, maxLen }; }
};

class TestEndOfTrackContext final : public IEndOfTrackContext
{
public:
    std::optional<int64_t> playEnd() const override { return end; }
    std::optional<int64_t> end;
};

struct Stereo {
    explicit Stereo(size_t len)
        : left(len), right(len) {}
    std::vector<float> left;
    std::vector<float> right;
    float* pointers[au::au4audio::numChannels] { left.data(), right.data() };
};

Stereo read(ISequenceProcessingNode& node, int64_t start, size_t len)
{
    Stereo stereo(len);
    node.GetSamples(stereo.pointers, start, len);
    return stereo;
}

std::vector<float> ramp(int64_t start, size_t len)
{
    std::vector<float> values(len);
    for (size_t i = 0; i < len; ++i) {
        values[i] = static_cast<float>(start + static_cast<int64_t>(i));
    }
    return values;
}
}

namespace {
//! The ramp of RampNode, silent from `silentFrom` on
std::vector<float> rampUntil(size_t len, size_t silentFrom)
{
    auto values = ramp(0, len);
    std::fill(values.begin() + static_cast<std::ptrdiff_t>(std::min(silentFrom, len)), values.end(), 0.f);
    return values;
}
}

TEST(EndOfTrackNodeTests, PlaysOnWithoutAnEnd)
{
    TestEndOfTrackContext context;
    EndOfTrackNode node { std::make_shared<RampNode>(), std::make_shared<LinearMapper>(), context };

    EXPECT_EQ(read(node, 0, TEST_READ_LEN).left, ramp(0, TEST_READ_LEN));
}

TEST(EndOfTrackNodeTests, SilencesPastThePlayEnd)
{
    TestEndOfTrackContext context;
    context.end = TEST_PLAY_END;
    EndOfTrackNode node { std::make_shared<RampNode>(), std::make_shared<LinearMapper>(), context };

    const auto output = read(node, 0, TEST_READ_LEN);

    const auto audible = static_cast<size_t>(TEST_PLAY_END - TEST_PLAYHEAD_START);
    EXPECT_EQ(output.left, rampUntil(TEST_READ_LEN, audible));
    EXPECT_EQ(output.right, rampUntil(TEST_READ_LEN, audible));
}

TEST(EndOfTrackNodeTests, SilencesWhereTheTrackStopsBeforeThePlayEnd)
{
    TestEndOfTrackContext context;
    context.end = TEST_PLAY_END;
    EndOfTrackNode node { std::make_shared<RampNode>(), std::make_shared<LinearMapper>(), context, TEST_TRACK_STOP };

    EXPECT_EQ(read(node, 0, TEST_READ_LEN).left, rampUntil(TEST_READ_LEN, static_cast<size_t>(TEST_TRACK_STOP - TEST_PLAYHEAD_START)));
}

TEST(EndOfTrackNodeTests, StopsTheTrackEvenWhileLooping)
{
    TestEndOfTrackContext context;
    EndOfTrackNode node { std::make_shared<RampNode>(), std::make_shared<LinearMapper>(), context, TEST_TRACK_STOP };

    EXPECT_EQ(read(node, 0, TEST_READ_LEN).left, rampUntil(TEST_READ_LEN, static_cast<size_t>(TEST_TRACK_STOP - TEST_PLAYHEAD_START)));
}
