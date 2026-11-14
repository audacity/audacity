/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <utility>
#include <vector>

#include "au3-clip-processing/clip-graph/ClipBufferingNode.h"

namespace {
constexpr size_t TEST_BUFFER_LENGTH = 8;
constexpr size_t TEST_READ_LENGTH = 3;
constexpr size_t TEST_NUM_READS = 10;

//! Its samples are their positions, and it records the reads
class RecordingNode final : public IClipProcessingNode
{
public:
    std::vector<std::pair<sampleCount, size_t> > reads;

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override
    {
        reads.emplace_back(start, len);
        for (size_t i = 0; i < len; ++i) {
            buffers[0][i] = static_cast<float>(start.as_double() + i);
        }
    }

    sampleCount NumSamples() const override { return 0; }
    size_t NChannels() const override { return 1; }
};

class ClipBufferingNodeTests : public ::testing::Test
{
protected:
    RecordingNode* m_input = new RecordingNode;
    ClipBufferingNode m_node { std::unique_ptr<IClipProcessingNode> { m_input }, TEST_BUFFER_LENGTH };

    //! Reads, and expects the samples' positions
    void read(sampleCount start, size_t len)
    {
        std::vector<float> samples(len);
        float* const buffers[] { samples.data() };
        m_node.GetSamples(buffers, start, len);
        for (size_t i = 0; i < len; ++i) {
            ASSERT_FLOAT_EQ(samples[i], start.as_double() + i) << "reading " << len << " from " << start.as_long_long();
        }
    }
};
}

TEST_F(ClipBufferingNodeTests, ReadingTheLastSamplesAgainDoesNotReadTheInputAgain)
{
    //! [GIVEN] reads that go back by the buffer's length, as a lookahead smoother does
    sampleCount position = 0;
    for (size_t i = 0; i < TEST_NUM_READS; ++i) {
        read(position, TEST_BUFFER_LENGTH);
        read(position + static_cast<sampleCount::type>(TEST_BUFFER_LENGTH), TEST_READ_LENGTH);
        position += static_cast<sampleCount::type>(TEST_READ_LENGTH);
    }

    //! [THEN] the input is read contiguously, each sample once
    sampleCount expectedStart = 0;
    for (const auto& [start, len] : m_input->reads) {
        EXPECT_EQ(start, expectedStart);
        expectedStart += static_cast<sampleCount::type>(len);
    }
    EXPECT_EQ(expectedStart, position + static_cast<sampleCount::type>(TEST_BUFFER_LENGTH));
}

TEST_F(ClipBufferingNodeTests, ReadingBeforeTheBufferReadsTheInputThere)
{
    //! [GIVEN] reads past the buffer's length
    read(0, 2 * TEST_BUFFER_LENGTH);

    //! [WHEN] reading earlier than what is buffered
    m_input->reads.clear();
    read(0, TEST_READ_LENGTH);

    //! [THEN] the input is read there
    ASSERT_EQ(m_input->reads.size(), 1);
    EXPECT_EQ(m_input->reads[0].first, 0);
    EXPECT_EQ(m_input->reads[0].second, TEST_READ_LENGTH);
}

TEST_F(ClipBufferingNodeTests, ReadingAfterTheBufferReadsTheInputThere)
{
    //! [GIVEN] a read
    read(0, TEST_BUFFER_LENGTH);

    //! [WHEN] reading further than where it ended
    m_input->reads.clear();
    const auto later = static_cast<sampleCount::type>(2 * TEST_BUFFER_LENGTH);
    read(later, TEST_READ_LENGTH);

    //! [THEN] the input is read there
    ASSERT_EQ(m_input->reads.size(), 1);
    EXPECT_EQ(m_input->reads[0].first, later);
}
