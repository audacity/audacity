/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <algorithm>
#include <cmath>
#include <numbers>
#include <vector>

#include "au3-clip-processing/sequence-graph/CrossFadingSmoother.h"

namespace {
constexpr size_t TEST_LOOKAHEAD = 8;
constexpr size_t TEST_NUM_CHANNELS = 1;
constexpr auto TEST_GAIN = 0.5;
constexpr auto TEST_OTHER_GAIN = 2.0;
constexpr auto TEST_THIRD_GAIN = -1.0;
constexpr size_t TEST_SIGNAL_LENGTH = 10 * TEST_LOOKAHEAD;
//! The smoother computes in single precision, the expectations in double precision
constexpr auto TEST_RELATIVE_TOLERANCE = 1e-6;

//! Its samples are their positions, so that the output tells which samples were read, times a gain, which can be
//! changed at any time
class GainNode final : public ISequenceProcessingNode
{
public:
    double gain = TEST_GAIN;

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override
    {
        for (size_t i = 0; i < len; ++i) {
            buffers[0][i] = static_cast<float>((start.as_double() + i) * gain);
        }
    }
};

class CrossFadingSmootherTests : public ::testing::Test
{
protected:
    GainNode* m_input = new GainNode;
    CrossFadingSmoother m_smoother { std::unique_ptr<ISequenceProcessingNode> { m_input }, TEST_NUM_CHANNELS, TEST_LOOKAHEAD };
    std::vector<float> m_output;

    //! Reads from where the previous read ended
    void read(size_t len)
    {
        std::vector<float> buffer(len);
        float* const buffers[] { buffer.data() };
        m_smoother.GetSamples(buffers, m_output.size(), len);
        m_output.insert(m_output.end(), buffer.begin(), buffer.end());
    }

    //! Reads blocks of `blockSize` until the output has `length` samples
    void readUpTo(size_t length, size_t blockSize)
    {
        while (m_output.size() < length) {
            read(std::min(blockSize, length - m_output.size()));
        }
    }

    //! The weight of the new gain at the `i`th sample of a cross-fade: a raised cosine
    static double weight(size_t i)
    {
        return (1 - std::cos(std::numbers::pi * i / TEST_LOOKAHEAD)) / 2;
    }

    void expectOutput(size_t n, double gain) const
    {
        ASSERT_NEAR(m_output[n], n * gain, std::abs(n * gain) * TEST_RELATIVE_TOLERANCE) << "at sample " << n;
    }
};
}

TEST_F(CrossFadingSmootherTests, WithoutChangesTheStreamIsProcessed)
{
    // Blocks shorter and longer than the lookahead
    for (const auto blockSize : { size_t { 1 }, TEST_LOOKAHEAD - 1, TEST_LOOKAHEAD, 3 * TEST_LOOKAHEAD }) {
        readUpTo(m_output.size() + TEST_SIGNAL_LENGTH, blockSize);
    }

    for (size_t n = 0; n < m_output.size(); ++n) {
        expectOutput(n, TEST_GAIN);
    }
}

TEST_F(CrossFadingSmootherTests, ReadingElsewhereIsNotCrossfaded)
{
    //! [GIVEN] some output
    constexpr auto blockSize = TEST_LOOKAHEAD / 2;
    constexpr auto jumpSample = TEST_SIGNAL_LENGTH / 2;
    readUpTo(jumpSample, blockSize);

    //! [WHEN] reading elsewhere, with the gain changed meanwhile, e.g. on a seek
    m_input->gain = TEST_OTHER_GAIN;
    constexpr auto elsewhere = 3 * TEST_SIGNAL_LENGTH;
    std::vector<float> buffer(TEST_SIGNAL_LENGTH);
    for (size_t position = 0; position < buffer.size(); position += blockSize) {
        float* const buffers[] { buffer.data() + position };
        m_smoother.GetSamples(buffers, elsewhere + position, blockSize);
    }

    //! [THEN] what is there is output as it is, e.g. so that a loop can be tuned to how it truly sounds
    for (size_t i = 0; i < buffer.size(); ++i) {
        const auto expected = (elsewhere + i) * TEST_OTHER_GAIN;
        ASSERT_NEAR(buffer[i], expected, expected * TEST_RELATIVE_TOLERANCE) << "at sample " << i;
    }
}

TEST_F(CrossFadingSmootherTests, FirstReadIsNotCrossfaded)
{
    //! [WHEN] reading for the first time, anywhere
    constexpr auto elsewhere = 3 * TEST_SIGNAL_LENGTH;
    std::vector<float> buffer(TEST_SIGNAL_LENGTH);
    float* const buffers[] { buffer.data() };
    m_smoother.GetSamples(buffers, elsewhere, buffer.size());

    //! [THEN] there is nothing to cross-fade from
    for (size_t i = 0; i < buffer.size(); ++i) {
        const auto expected = (elsewhere + i) * TEST_GAIN;
        ASSERT_NEAR(buffer[i], expected, expected * TEST_RELATIVE_TOLERANCE) << "at sample " << i;
    }
}
