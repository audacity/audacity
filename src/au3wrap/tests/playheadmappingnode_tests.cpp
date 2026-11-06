/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <algorithm>
#include <optional>
#include <utility>
#include <vector>

#include "au3-clip-processing/sequence-graph/PlayheadMappingNode.h"

namespace {
constexpr size_t TEST_NUM_CHANNELS = 1;
//! Where the stream starts in the input, so that positions differ from indices
constexpr long long TEST_START_POSITION = 1000;
constexpr size_t TEST_READ_LEN = 64;

//! Its samples are their positions
class PositionNode final : public ISequenceProcessingNode
{
public:
    void GetSamples(float* const* buffers, sampleCount start, size_t len) override
    {
        for (size_t i = 0; i < len; ++i) {
            buffers[0][i] = static_cast<float>(start.as_double() + i);
        }
    }
};

//! The stream plays the input linearly from `TEST_START_POSITION`, and loops `loop` once it reaches it, if given
class TestMapper final : public IPlayheadMapper
{
public:
    explicit TestMapper(std::optional<std::pair<long long, long long> > loop = {})
        : m_loop{loop} {}

    Segment Map(sampleCount index, size_t maxLen) const override
    {
        const auto position = Position(index.as_long_long());
        const auto len = m_loop ? std::min<size_t>(maxLen, m_loop->second - position) : maxLen;
        return { position, len };
    }

    long long Position(long long index) const
    {
        const auto linear = TEST_START_POSITION + index;
        if (!m_loop || linear < m_loop->first) {
            return linear;
        }
        return m_loop->first + (linear - m_loop->first) % (m_loop->second - m_loop->first);
    }

private:
    const std::optional<std::pair<long long, long long> > m_loop;
};

std::vector<float> read(ISequenceProcessingNode& node, long long start, size_t len)
{
    std::vector<float> buffer(len);
    float* const buffers[] { buffer.data() };
    node.GetSamples(buffers, start, len);
    return buffer;
}

//! What the playhead reads, sample by sample
std::vector<float> mapped(const TestMapper& mapper, long long start, size_t len)
{
    std::vector<float> result(len);
    for (size_t i = 0; i < len; ++i) {
        result[i] = static_cast<float>(mapper.Position(start + static_cast<long long>(i)));
    }
    return result;
}
}

TEST(PlayheadMappingNodeTests, readsGetTheInputWhereThePlayheadReads)
{
    const auto mapper = std::make_shared<TestMapper>();
    PlayheadMappingNode node { std::make_shared<PositionNode>(), mapper, TEST_NUM_CHANNELS };
    EXPECT_EQ(read(node, 0, TEST_READ_LEN), mapped(*mapper, 0, TEST_READ_LEN));
}

TEST(PlayheadMappingNodeTests, readsWrapAroundLoopsAsOftenAsMapped)
{
    //! A loop that a read wraps around several times
    constexpr long long loopStart = TEST_START_POSITION + TEST_READ_LEN / 8;
    constexpr long long loopEnd = loopStart + TEST_READ_LEN / 5;
    const auto loop = std::make_pair(loopStart, loopEnd);
    const auto mapper = std::make_shared<TestMapper>(loop);
    PlayheadMappingNode node { std::make_shared<PositionNode>(), mapper, TEST_NUM_CHANNELS };
    EXPECT_EQ(read(node, 0, TEST_READ_LEN), mapped(*mapper, 0, TEST_READ_LEN));
}
