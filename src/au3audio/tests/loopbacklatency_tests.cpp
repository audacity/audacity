/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <cstdint>
#include <vector>

#include "au3-audio-io/LoopbackLatency.h"

namespace au::au3audio {
namespace {
constexpr size_t PERIOD = 48000;
constexpr size_t PERIODS = 4;

//! The burst train as played, delayed by `delay` frames (plus `driftPerPeriod`
//! more for each following period), scaled by `gain` and mixed with noise
std::vector<float> makeCapture(const std::vector<float>& burst, size_t delay, float gain, float noise, size_t driftPerPeriod = 0)
{
    std::vector<float> capture(PERIOD * PERIODS, 0.0f);
    uint32_t seed = 42;
    for (float& sample : capture) {
        seed = seed * 1664525u + 1013904223u;
        sample = ((seed >> 8) / float(1 << 24) - 0.5f) * noise;
    }
    for (size_t k = 0; k < PERIODS; ++k) {
        const size_t start = k * PERIOD + delay + k * driftPerPeriod;
        for (size_t j = 0; j < burst.size() && start + j < capture.size(); ++j) {
            capture[start + j] += gain * burst[j];
        }
    }
    return capture;
}
}

TEST(LoopbackLatencyTests, FindsExactDelay)
{
    const auto burst = LoopbackLatency::MakeBurst(0.25f);
    for (const size_t delay : { size_t(0), size_t(1), size_t(37), size_t(979), size_t(21000), PERIOD - burst.size() - 1 }) {
        const auto analysis = LoopbackLatency::Analyse(makeCapture(burst, delay, 0.2f, 0.1f), burst, PERIOD);
        EXPECT_EQ(analysis.roundTripFrames, static_cast<long>(delay)) << "delay " << delay;
        EXPECT_EQ(analysis.burstsFound, PERIODS) << "delay " << delay;
        EXPECT_EQ(analysis.spreadFrames, 0) << "delay " << delay;
        EXPECT_FALSE(analysis.inverted);
    }
}

TEST(LoopbackLatencyTests, ReportsInvertedPolarity)
{
    const auto burst = LoopbackLatency::MakeBurst(0.25f);
    const auto analysis = LoopbackLatency::Analyse(makeCapture(burst, 500, -0.2f, 0.05f), burst, PERIOD);
    EXPECT_EQ(analysis.roundTripFrames, 500);
    EXPECT_TRUE(analysis.inverted);
}

TEST(LoopbackLatencyTests, FindsNothingInNoise)
{
    const auto burst = LoopbackLatency::MakeBurst(0.25f);
    const auto analysis = LoopbackLatency::Analyse(makeCapture(burst, 500, 0.0f, 0.1f), burst, PERIOD);
    EXPECT_EQ(analysis.burstsFound, 0u);
    EXPECT_EQ(analysis.roundTripFrames, -1);
}

TEST(LoopbackLatencyTests, ShowsClockDriftAsSpread)
{
    const auto burst = LoopbackLatency::MakeBurst(0.25f);
    const auto analysis = LoopbackLatency::Analyse(makeCapture(burst, 1000, 0.2f, 0.05f, 2), burst, PERIOD);
    EXPECT_EQ(analysis.burstsFound, PERIODS);
    EXPECT_EQ(analysis.spreadFrames, 2 * static_cast<long>(PERIODS - 1));
}
}
